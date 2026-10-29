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
    , rate : IntervalRate
    , bells : List Int
    , seed : Random.Seed
    }


type Bell
    = Starting
    | Interval Int
    | Final


type IntervalRate
    = NoIntervals
    | LessOften
    | MoreOften


type alias Configuration =
    { duration : Int -- seconds
    , warmup : Maybe Int -- seconds
    , rate : IntervalRate
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
    , rate = MoreOften
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
    , rate = config.rate
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
            ( count, seed ) =
                Random.step (bellCount timer.rate timer.duration) timer.seed

            next =
                planBells count
                    (timer.warmup + minGap)
                    { timer | phase = Meditating, seed = seed, now = Just now }
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


bellCount : IntervalRate -> Int -> Random.Generator Int
bellCount rate duration =
    let
        scale =
            sqrt (toFloat duration / toFloat (10 * 60 * 1000))

        between lo hi =
            Random.int (max 1 (round (lo * scale))) (max 1 (round (hi * scale)))
    in
    case rate of
        NoIntervals ->
            Random.constant 0

        LessOften ->
            between 1 2

        MoreOften ->
            between 2 4


placeBells : Int -> Int -> Int -> Random.Generator (List Int)
placeBells count from to =
    let
        fits =
            if to < from then
                0

            else
                (to - from) // minGap + 1

        n =
            min count fits

        slack =
            to - from - (n - 1) * minGap
    in
    Random.list n (Random.int 0 slack)
        |> Random.map (List.sort >> List.indexedMap (\i t -> from + t + i * minGap))


planBells : Int -> Int -> TimerState -> TimerState
planBells count from timer =
    let
        ( bells, seed ) =
            Random.step (placeBells count from (totalMillis timer - endGuard)) timer.seed
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
                    { base | status = TimerStarted, startedAt = Just now, now = Just now }
                        |> (if resuming && base.phase == Meditating then
                                planBells (List.length base.bells) (elapsed + minGap)

                            else
                                identity
                           )

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


endGuard : Int
endGuard =
    60 * 1000


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


remaining : TimerState -> Int -> Int
remaining timer elapsed =
    if elapsed < timer.warmup then
        timer.warmup - elapsed

    else
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
    , accent = rgb255 0xF3 0x8B 0xA8
    }


timerDisplay : Model -> Element Msg
timerDisplay model =
    let
        timer =
            model.timer

        elapsed =
            case timer.now of
                Just now ->
                    elapsedMillis timer now

                Nothing ->
                    timer.elapsedBefore
    in
    el [ centerX, Font.size 64, Font.variant Font.tabularNumbers ]
        (remaining timer elapsed
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
                    ( "Restart", Start )
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
            , timerDisplay model
            , row [ centerX, spacing 16 ]
                [ timerButton model
                ]
            ]
        )
