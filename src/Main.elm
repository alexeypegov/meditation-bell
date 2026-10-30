port module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..))
import Element exposing (Element, centerX, centerY, column, el, padding, paddingXY, rgb255, row, spacing, text)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Process
import Random
import Task
import Time



-- PORTS


port playBell : String -> Cmd msg


port stopBells : () -> Cmd msg


port keepAwake : Bool -> Cmd msg


port pageHidden : (() -> msg) -> Sub msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type TimerStatus
    = TimerReady
    | TimerStarted
    | TimerPaused
    | TimerFinished


type Phase
    = Preparing
    | Meditating


type alias TimerState =
    { status : TimerStatus
    , warmup : Int -- ms
    , duration : Int -- ms
    , elapsedBefore : Int -- ms accumulated before the current run (for pause/resume)
    , startedAt : Maybe Time.Posix
    , now : Maybe Time.Posix
    , phase : Phase
    , intervals : Bool
    , bells : List Int
    , seed : Random.Seed
    }


type Bell
    = Starting
    | Interval Int
    | Final


type alias Configuration =
    { duration : Int -- seconds
    , warmup : Maybe Int -- seconds
    , intervals : Bool
    }


type alias Model =
    { timer : TimerState
    , config : Configuration
    }


{-| Number of interval sounds, as public/sounds/interval-1.mp3 ... interval-N.mp3
-}
intervalCount : Int
intervalCount =
    3


initialConfig : Configuration
initialConfig =
    { duration = 900
    , warmup = Just 60
    , intervals = True
    }


freshTimer : Configuration -> TimerState
freshTimer config =
    { status = TimerReady
    , warmup = Maybe.withDefault 0 config.warmup * 1000
    , duration = config.duration * 1000
    , elapsedBefore = 0
    , startedAt = Nothing
    , now = Nothing
    , phase = Preparing
    , intervals = config.intervals
    , bells = []
    , seed = Random.initialSeed 0
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { timer = freshTimer initialConfig
      , config = initialConfig
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Start
    | StartedAt Time.Posix
    | Pause
    | PausedAt Time.Posix
    | Reset
    | Tick Time.Posix
    | VisibilityChanged Visibility
    | PageHidden


advance : Time.Posix -> Model -> ( Model, Cmd Msg )
advance now model =
    let
        timer =
            model.timer

        elapsed =
            elapsedMillis timer now
    in
    if elapsed >= totalMillis timer then
        ( { model
            | timer =
                { timer
                    | status = TimerFinished
                    , phase = Meditating
                    , elapsedBefore = totalMillis timer
                    , startedAt = Nothing
                    , now = Just now
                }
          }
        , Cmd.batch [ keepAwake False, ringBell Final ]
        )

    else if timer.phase == Preparing && elapsed >= timer.warmup then
        let
            next =
                planBells { timer | phase = Meditating, now = Just now }
        in
        ( { model | timer = next }
        , Cmd.batch [ ringBell Starting, scheduleBell next elapsed ]
        )

    else if isDue timer.bells elapsed then
        let
            ( sound, seed ) =
                Random.step (Random.int 1 intervalCount) timer.seed

            next =
                { timer | bells = List.drop 1 timer.bells, seed = seed, now = Just now }
        in
        ( { model | timer = next }
        , Cmd.batch [ ringBell (Interval sound), scheduleBell next elapsed ]
        )

    else
        ( { model | timer = { timer | now = Just now } }, Cmd.none )


pauseIfRunning : Model -> ( Model, Cmd Msg )
pauseIfRunning model =
    case model.timer.status of
        TimerStarted ->
            ( model, Task.perform PausedAt Time.now )

        _ ->
            ( model, Cmd.none )


bellCount : TimerState -> Random.Generator Int
bellCount timer =
    if timer.intervals then
        let
            minutes =
                toFloat timer.duration / 60000

            least =
                clamp 1 maxBells (round (1.5 * sqrt (minutes / 10)))
        in
        Random.int least maxBells

    else
        Random.constant 0


{-| Places `left` bells at random between `start` and `end`, none closer than
`minGap` to any bell in `taken` (which starts with the start and final bells).
A bell that can't find room within its tries is dropped.
-}
placeNext : Int -> Int -> List Int -> Int -> Int -> Random.Generator (List Int)
placeNext left tries taken start end =
    if left <= 0 then
        Random.constant taken

    else
        Random.int start end
            |> Random.andThen
                (\t ->
                    if List.all (\b -> abs (t - b) >= minGap) taken then
                        placeNext (left - 1) bellTries (t :: taken) start end

                    else if tries > 1 then
                        placeNext left (tries - 1) taken start end

                    else
                        placeNext (left - 1) bellTries taken start end
                )


planBells : TimerState -> TimerState
planBells timer =
    let
        start =
            timer.warmup

        end =
            totalMillis timer

        plan =
            bellCount timer
                |> Random.andThen (\count -> placeNext count firstTries [ start, end ] start end)
                |> Random.map (List.filter (\t -> t /= start && t /= end) >> List.sort)

        ( bells, seed ) =
            Random.step plan timer.seed
    in
    { timer | bells = bells, seed = seed }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        timer =
            model.timer
    in
    case msg of
        Start ->
            ( model, Task.perform StartedAt Time.now )

        StartedAt now ->
            let
                resuming =
                    timer.status == TimerPaused

                base =
                    if resuming then
                        timer

                    else
                        let
                            fresh =
                                freshTimer model.config
                        in
                        { fresh | seed = Random.initialSeed (Time.posixToMillis now) }

                elapsed =
                    elapsedMillis { base | startedAt = Just now } now

                started =
                    { base
                        | status = TimerStarted
                        , startedAt = Just now
                        , now = Just now
                        , bells = List.filter (\at -> at >= elapsed + minGap) base.bells
                    }

                wakeUps =
                    [ started.warmup - elapsed, totalMillis started - elapsed ]
                        |> List.filter (\ms -> ms > 0)
                        |> List.map scheduleTick

                ( advanced, bellCmd ) =
                    advance now { model | timer = started }
            in
            ( advanced, Cmd.batch (keepAwake True :: bellCmd :: scheduleBell started elapsed :: wakeUps) )

        Pause ->
            pauseIfRunning model

        PausedAt now ->
            if timer.status == TimerStarted then
                ( { model
                    | timer =
                        { timer
                            | status = TimerPaused
                            , elapsedBefore = elapsedMillis timer now
                            , startedAt = Nothing
                            , now = Just now
                        }
                  }
                , Cmd.batch [ keepAwake False, stopBells () ]
                )

            else
                ( model, Cmd.none )

        Reset ->
            ( { model | timer = freshTimer model.config }
            , Cmd.batch [ keepAwake False, stopBells () ]
            )

        Tick now ->
            if timer.status == TimerStarted then
                advance now model

            else
                ( model, Cmd.none )

        VisibilityChanged Hidden ->
            pauseIfRunning model

        VisibilityChanged Visible ->
            ( model, Cmd.none )

        PageHidden ->
            pauseIfRunning model



-- SUBSCRIPTIONS


tick : Model -> Sub Msg
tick model =
    case model.timer.status of
        TimerStarted ->
            Time.every 1000 Tick

        _ ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onVisibilityChange VisibilityChanged
        , pageHidden (\_ -> PageHidden)
        , tick model
        ]



-- HELPERS


isDue : List Int -> Int -> Bool
isDue bells elapsed =
    case bells of
        at :: _ ->
            elapsed >= at

        [] ->
            False


scheduleBell : TimerState -> Int -> Cmd Msg
scheduleBell timer elapsed =
    case timer.bells of
        at :: _ ->
            scheduleTick (at - elapsed)

        [] ->
            Cmd.none


maxBells : Int
maxBells =
    7


{-| Tries per bell before it's dropped; the first bell gets many, so a session
always has at least one interval bell when one can fit.
-}
bellTries : Int
bellTries =
    3


firstTries : Int
firstTries =
    100


minGap : Int
minGap =
    60 * 1000


bellName : Bell -> String
bellName bell =
    case bell of
        Starting ->
            "starting"

        Interval n ->
            "interval-" ++ String.fromInt n

        Final ->
            "final"


ringBell : Bell -> Cmd msg
ringBell bell =
    playBell (bellName bell)


elapsedMillis : TimerState -> Time.Posix -> Int
elapsedMillis timer now =
    case timer.startedAt of
        Just started ->
            timer.elapsedBefore + (Time.posixToMillis now - Time.posixToMillis started)

        Nothing ->
            timer.elapsedBefore


totalMillis : TimerState -> Int
totalMillis timer =
    timer.warmup + timer.duration


remaining : Phase -> TimerState -> Int -> Int
remaining phase timer elapsed =
    case phase of
        Preparing ->
            clamp 0 timer.warmup (timer.warmup - elapsed)

        Meditating ->
            clamp 0 timer.duration (totalMillis timer - elapsed)


{-| Fires a Tick after `ms`, used to ring bells exactly on time.
-}
scheduleTick : Int -> Cmd Msg
scheduleTick ms =
    Process.sleep (toFloat ms)
        |> Task.andThen (\_ -> Time.now)
        |> Task.perform Tick


formatTime : Int -> String
formatTime ms =
    let
        totalSeconds =
            ceiling (toFloat ms / 1000)

        hours =
            totalSeconds // 3600

        minutes =
            modBy 60 (totalSeconds // 60)

        seconds =
            modBy 60 totalSeconds

        pad2 n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    if hours > 0 then
        String.fromInt hours ++ ":" ++ pad2 minutes ++ ":" ++ pad2 seconds

    else
        String.fromInt minutes ++ ":" ++ pad2 seconds



-- VIEW


colors =
    { bg = rgb255 0x1E 0x1E 0x2E
    , fg = rgb255 0xCD 0xD6 0xF4
    , inactive = rgb255 0x8B 0x8B 0x8B
    , accent = rgb255 0xF3 0x8B 0xA8
    }


timerDisplay : Phase -> Model -> Element Msg
timerDisplay phase model =
    let
        timer =
            model.timer

        elapsed =
            case timer.now of
                Just now ->
                    elapsedMillis timer now

                Nothing ->
                    timer.elapsedBefore

        fontSize =
            case phase of
                Preparing ->
                    36

                Meditating ->
                    64

        remainingTime =
            remaining phase timer elapsed

        foregroundColor =
            case phase of
                Preparing ->
                    if remainingTime > 0 then
                        colors.fg

                    else
                        colors.bg

                Meditating ->
                    colors.fg
    in
    el [ centerX, Font.size fontSize, Font.color foregroundColor, Font.variant Font.tabularNumbers ]
        (remainingTime
            |> formatTime
            |> text
        )


timerButton : Model -> Element Msg
timerButton model =
    let
        ( label, msg ) =
            case model.timer.status of
                TimerReady ->
                    ( "Start", Start )

                TimerStarted ->
                    ( "Pause", Pause )

                TimerPaused ->
                    ( "Resume", Start )

                TimerFinished ->
                    ( "Start", Start )
    in
    Input.button
        [ Background.color colors.accent
        , paddingXY 24 12
        ]
        { onPress = Just msg
        , label = text label
        }


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color colors.bg
        , Font.color colors.fg
        , Font.family [ Font.typeface "system-ui", Font.sansSerif ]
        ]
        (column [ centerX, centerY, spacing 24, padding 16 ]
            [ el [ centerX, Font.size 32, Font.semiBold ] (text "Meditation Timer")
            , timerDisplay Preparing model
            , timerDisplay Meditating model
            , row [ centerX, spacing 16 ]
                [ timerButton model
                ]
            ]
        )
