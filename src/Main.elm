port module Main exposing (main)

import Browser
import Element exposing (Attribute, Element, alignBottom, alignTop, centerX, centerY, column, el, padding, paddingXY, rgb255, row, spacing, spacingXY, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Process
import Random
import Svg
import Svg.Attributes as SA
import Task
import Time



-- PORTS


port playBell : String -> Cmd msg


port stopBells : () -> Cmd msg


port keepAwake : { session : Bool, screen : Bool } -> Cmd msg


port saveSettings : Encode.Value -> Cmd msg



-- MAIN


main : Program (Maybe String) Model Msg
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
    , warmup : Int -- seconds
    , intervals : Bool
    , keepScreenOn : Bool
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
    { duration = 600
    , warmup = 30
    , intervals = True
    , keepScreenOn = False
    }


freshTimer : Configuration -> TimerState
freshTimer config =
    { status = TimerReady
    , warmup = config.warmup * 1000
    , duration = config.duration * 1000
    , elapsedBefore = 0
    , startedAt = Nothing
    , now = Nothing
    , phase = Preparing
    , intervals = config.intervals
    , bells = []
    , seed = Random.initialSeed 0
    }


init : Maybe String -> ( Model, Cmd Msg )
init stored =
    let
        config =
            stored
                |> Maybe.andThen (Decode.decodeString configDecoder >> Result.toMaybe)
                |> Maybe.withDefault initialConfig
    in
    ( { timer = freshTimer config
      , config = config
      }
    , Cmd.none
    )



-- DECODERS


warmupDecoder : Decoder Int
warmupDecoder =
    Decode.int
        |> Decode.andThen
            (\warmup ->
                if List.member warmup warmupDurations then
                    Decode.succeed warmup

                else
                    Decode.fail "Unknown warmup"
            )


{-| Missing or invalid fields fall back to `initialConfig`, so adding a setting
later doesn't throw away the ones already saved.
-}
configDecoder : Decoder Configuration
configDecoder =
    let
        field name decoder default =
            Decode.oneOf [ Decode.field name decoder, Decode.succeed default ]
    in
    Decode.map4 Configuration
        (field "duration" (Decode.map (clamp minDuration maxDuration) Decode.int) initialConfig.duration)
        (field "warmup" warmupDecoder initialConfig.warmup)
        (field "intervals" Decode.bool initialConfig.intervals)
        (field "keepScreenOn" Decode.bool initialConfig.keepScreenOn)


encodeConfig : Configuration -> Encode.Value
encodeConfig config =
    Encode.object
        [ ( "duration", Encode.int config.duration )
        , ( "warmup", Encode.int config.warmup )
        , ( "intervals", Encode.bool config.intervals )
        , ( "keepScreenOn", Encode.bool config.keepScreenOn )
        ]


minDuration : Int
minDuration =
    5 * 60


maxDuration : Int
maxDuration =
    60 * 60


warmupDurations : List Int
warmupDurations =
    [ 0, 30, 60, 120, 180, 300 ]


setConfig : Configuration -> Model -> ( Model, Cmd Msg )
setConfig config model =
    ( { model | config = config, timer = freshTimer config }
    , saveSettings (encodeConfig config)
    )



-- UPDATE


type Msg
    = Start
    | StartedAt Time.Posix
    | Pause
    | PausedAt Time.Posix
    | Reset
    | Heartbeat Time.Posix
    | Tick Time.Posix
    | ChangeDuration Int
    | CycleWarmup
    | ToggleScreenOn
    | ToggleIntervalBells


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
            | timer = freshTimer model.config
          }
        , Cmd.batch [ awake False model, ringBell Final ]
        )

    else if timer.phase == Preparing && elapsed >= timer.warmup then
        let
            next =
                planBells { timer | phase = Meditating, now = Just now }
        in
        ( { model | timer = next }
        , ringBell Starting
        )

    else if isDue timer.bells elapsed then
        let
            ( sound, seed ) =
                Random.step (Random.int 1 intervalCount) timer.seed

            next =
                { timer | bells = List.drop 1 timer.bells, seed = seed, now = Just now }
        in
        ( { model | timer = next }
        , ringBell (Interval sound)
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


awake : Bool -> Model -> Cmd msg
awake on model =
    keepAwake { session = on, screen = on && model.config.keepScreenOn }


{-| Places `left` bells at random between `start` and `end`, none closer than
`minGap` to any bell in `taken` (which starts with the start and final bells).
A bell that can't find room within its tries is dropped.
-}
placeNext : Int -> Int -> List Int -> Int -> Int -> Random.Generator (List Int)
placeNext left tries taken start end =
    if left <= 0 then
        Random.constant taken

    else
        Random.int (start // 1000) (end // 1000)
            |> Random.map ((*) 1000)
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

                ( advanced, bellCmd ) =
                    advance now { model | timer = started }
            in
            ( advanced, Cmd.batch [ awake True model, bellCmd, exactTicks elapsed ] )

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
                , Cmd.batch [ awake False model, stopBells () ]
                )

            else
                ( model, Cmd.none )

        Reset ->
            ( { model | timer = freshTimer model.config }
            , Cmd.batch [ awake False model, stopBells () ]
            )

        Heartbeat now ->
            if timer.status == TimerStarted then
                let
                    ( advanced, cmd ) =
                        advance now model
                in
                ( advanced, Cmd.batch [ cmd, exactTicks (elapsedMillis timer now) ] )

            else
                ( model, Cmd.none )

        Tick now ->
            if timer.status == TimerStarted then
                advance now model

            else
                ( model, Cmd.none )

        ChangeDuration step ->
            if editable timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | duration = clamp minDuration maxDuration (config.duration + step) } model

            else
                ( model, Cmd.none )

        CycleWarmup ->
            if editable timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | warmup = nextWarmup config.warmup } model

            else
                ( model, Cmd.none )

        ToggleScreenOn ->
            if editable timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | keepScreenOn = not config.keepScreenOn } model

            else
                ( model, Cmd.none )

        ToggleIntervalBells ->
            if editable timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | intervals = not config.intervals } model

            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


heartbeat : Model -> Sub Msg
heartbeat model =
    case model.timer.status of
        TimerStarted ->
            Time.every 1000 Heartbeat

        _ ->
            Sub.none


subscriptions : Model -> Sub Msg
subscriptions model =
    heartbeat model



-- HELPERS


isDue : List Int -> Int -> Bool
isDue bells elapsed =
    case bells of
        at :: _ ->
            elapsed >= at

        [] ->
            False


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


{-| Exact updates on the next two seconds of the timeline. The heartbeat
drifts, so it only schedules these; they never schedule anything themselves,
which keeps one left over from before a pause harmless.
-}
exactTicks : Int -> Cmd Msg
exactTicks elapsed =
    let
        toNext =
            1000 - modBy 1000 elapsed

        at ms =
            Process.sleep (toFloat (ms + 5))
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform Tick
    in
    Cmd.batch [ at toNext, at (toNext + 1000) ]


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


formatTime : Int -> String
formatTime ms =
    let
        totalSeconds =
            ceiling (toFloat ms / 1000)

        minutes =
            totalSeconds // 60

        seconds =
            modBy 60 totalSeconds

        pad2 n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    String.fromInt minutes ++ ":" ++ pad2 seconds


formatCountdown : Int -> String
formatCountdown ms =
    let
        secondsLeft =
            ceiling (toFloat ms / 1000)
    in
    if secondsLeft < 60 then
        String.fromInt secondsLeft

    else
        formatTime ms



-- VIEW


{-| Eldritch "Cthulhu" palette, <https://github.com/eldritch-theme/eldritch>
-}
eldritch =
    { sunkenDepths = rgb255 0x21 0x23 0x37
    , shallowDepths = rgb255 0x32 0x34 0x49
    , tidalSurface = rgb255 0x45 0x47 0x59
    , murkOverlay = rgb255 0x5B 0x5C 0x66
    , lighthouseWhite = rgb255 0xEB 0xFA 0xFA
    , oldOnePurple = rgb255 0x70 0x81 0xD0
    , wateryTombBlue = rgb255 0x04 0xD1 0xF9
    , greatOldOneGreen = rgb255 0x37 0xF4 0x99
    , dreamingOrange = rgb255 0xF7 0xC6 0x7F
    , pustulePink = rgb255 0xF2 0x65 0xB5
    , lovecraftPurple = rgb255 0xA4 0x8C 0xF2
    , rlyehRed = rgb255 0xF1 0x6C 0x75
    , goldOfYuggoth = rgb255 0xF1 0xFC 0x79
    }


colors =
    { bg = eldritch.sunkenDepths
    , surface = eldritch.shallowDepths
    , fg = eldritch.lighthouseWhite
    , inactive = eldritch.oldOnePurple
    , disabled = eldritch.murkOverlay
    , accent = eldritch.greatOldOneGreen
    }


icon : Bool -> List (Svg.Svg msg) -> Element msg
icon filled shapes =
    let
        paint =
            if filled then
                [ SA.fill "currentColor" ]

            else
                [ SA.fill "none", SA.stroke "currentColor", SA.strokeWidth "1.4", SA.strokeLinejoin "round" ]
    in
    Element.html
        (Svg.svg [ SA.viewBox "0 0 24 24", SA.width "26", SA.height "26" ]
            [ Svg.g paint shapes ]
        )


bellIcon : Bool -> Element msg
bellIcon filled =
    icon filled
        [ Svg.path [ SA.d "M10.3 3.7V3.2a1.7 1.7 0 0 1 3.4 0v.5z" ] []
        , Svg.path [ SA.d "M12 4.3C8.7 4.3 6 6.8 6 10.1V13.2C6 15.2 4.8 17 3.6 18.2C2.9 18.9 3.3 19.8 4.2 19.8H19.8C20.7 19.8 21.1 18.9 20.4 18.2C19.2 17 18 15.2 18 13.2V10.1C18 6.8 15.3 4.3 12 4.3Z" ] []
        , Svg.path [ SA.d "M9.3 20.6h5.4a2.7 2.3 0 0 1-5.4 0z" ] []
        ]


phoneIcon : Bool -> Element msg
phoneIcon lit =
    let
        frame =
            Svg.rect
                [ SA.x "5"
                , SA.y "1.5"
                , SA.width "14"
                , SA.height "21"
                , SA.rx "3.2"
                , SA.fill "none"
                , SA.stroke "currentColor"
                , SA.strokeWidth "1.4"
                ]
                []

        screen =
            Svg.rect
                [ SA.x "7.2"
                , SA.y "3.7"
                , SA.width "9.6"
                , SA.height "16.6"
                , SA.rx "1"
                , SA.fill "currentColor"
                ]
                []
    in
    Element.html
        (Svg.svg [ SA.viewBox "0 0 24 24", SA.width "26", SA.height "26" ]
            (if lit then
                [ frame, screen ]

             else
                [ frame ]
            )
        )


durationStep : Int
durationStep =
    5 * 60


plusIcon : Element msg
plusIcon =
    strokeIcon [ "M12 6v12", "M6 12h12" ]


minusIcon : Element msg
minusIcon =
    strokeIcon [ "M6 12h12" ]


strokeIcon : List String -> Element msg
strokeIcon paths =
    Element.html
        (Svg.svg [ SA.viewBox "0 0 24 24", SA.width "24", SA.height "24" ]
            [ Svg.g [ SA.fill "none", SA.stroke "currentColor", SA.strokeWidth "2", SA.strokeLinecap "round" ]
                (List.map (\d -> Svg.path [ SA.d d ] []) paths)
            ]
        )


stepButton : Float -> Maybe Msg -> Element Msg -> Element Msg
stepButton opacity onPress iconEl =
    let
        pressed =
            case onPress of
                Just _ ->
                    [ Element.mouseDown [ Background.color eldritch.tidalSurface ] ]

                Nothing ->
                    []
    in
    Input.button
        ([ Element.width (Element.px 48)
         , Element.height (Element.px 48)
         , Border.rounded 24
         , Background.color colors.surface
         , Font.color colors.inactive
         , Element.alpha opacity
         , Element.focused []
         , style "transition" "opacity 0.3s"
         ]
            ++ pressed
        )
        { onPress = onPress
        , label = el [ centerX, centerY ] iconEl
        }


durationControl : Model -> Element Msg
durationControl model =
    let
        config =
            model.config

        counting =
            not isEditable

        isEditable =
            editable model.timer.status

        step delta symbol =
            let
                target =
                    config.duration + delta
            in
            if not isEditable then
                stepButton 0 Nothing symbol

            else if target >= minDuration && target <= maxDuration then
                stepButton 1 (Just (ChangeDuration delta)) symbol

            else
                stepButton 0.3 Nothing symbol

        side =
            el [ centerY, paddingXY 16 0 ]

        left =
            remaining Meditating model.timer (currentElapsed model.timer)

        color =
            if counting && model.timer.phase == Preparing then
                colors.disabled

            else
                colors.fg
    in
    el
        [ centerX
        , Element.onLeft (side (step (negate durationStep) minusIcon))
        , Element.onRight (side (step durationStep plusIcon))
        ]
        (el
            [ Font.size 72
            , Font.variant Font.tabularNumbers
            , Font.color color
            , Font.center
            , style "min-width" "5ch"
            , style "transition" "color 0.3s"
            ]
            (timeText model left)
        )


timerButton : Model -> Element Msg
timerButton model =
    let
        running =
            model.timer.status == TimerStarted

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
        [ paddingXY 36 16
        , Background.color
            (if running then
                Element.rgba 0 0 0 0

             else
                colors.surface
            )
        , Font.color
            (if running then
                colors.inactive

             else
                colors.accent
            )
        , Font.size 32
        , Border.rounded 22
        , style "transition" "background-color 0.3s, color 0.3s"
        , Element.focused []
        ]
        { onPress = Just msg
        , label = text label
        }


toggleControl : Model -> Bool -> (Bool -> Element Msg) -> Msg -> Element Msg
toggleControl model on iconFor msg =
    let
        visible =
            editable model.timer.status
    in
    Input.button
        [ Element.width (Element.px 48)
        , Element.height (Element.px 48)
        , Border.rounded 24
        , Background.color colors.surface
        , Element.mouseDown [ Background.color eldritch.tidalSurface ]
        , Font.color
            (if on then
                colors.fg

             else
                colors.inactive
            )
        , Element.alpha
            (if visible then
                1

             else
                0
            )
        , Element.focused []
        , style "transition" "opacity 0.3s, color 0.2s"
        ]
        { onPress =
            if visible then
                Just msg

            else
                Nothing
        , label = el [ centerX, centerY ] (iconFor on)
        }


nextWarmup : Int -> Int
nextWarmup current =
    warmupDurations
        |> List.filter (\w -> w > current)
        |> List.head
        |> Maybe.withDefault 0


editable : TimerStatus -> Bool
editable status =
    status == TimerReady || status == TimerFinished


currentElapsed : TimerState -> Int
currentElapsed timer =
    case timer.now of
        Just now ->
            elapsedMillis timer now

        Nothing ->
            timer.elapsedBefore


timeText : Model -> Int -> Element Msg
timeText model ms =
    let
        formatted =
            if editable model.timer.status then
                formatTime ms

            else
                formatCountdown ms
    in
    text formatted


warmupControl : Model -> Element Msg
warmupControl model =
    let
        transparent =
            Element.rgba 0 0 0 0

        timer =
            model.timer

        isEditable =
            editable timer.status

        left =
            remaining Preparing timer (currentElapsed timer)

        label =
            if model.config.warmup == 0 then
                text (formatTime 0)

            else if isEditable then
                timeText model (model.config.warmup * 1000)

            else
                timeText model left

        color =
            if isEditable then
                colors.inactive

            else
                colors.fg

        opacity =
            if isEditable then
                1

            else if left <= 1000 then
                0

            else
                0.4 + 0.6 * clamp 0 1 (toFloat (left - 1000) / 5000)

        buttonStyle =
            if isEditable then
                [ Background.color colors.surface
                , Border.color eldritch.tidalSurface
                , Element.mouseDown [ Background.color eldritch.tidalSurface ]
                ]

            else
                [ Background.color transparent
                , Border.color transparent
                ]
    in
    el [ centerX ]
        (Input.button
            ([ centerX
             , paddingXY 20 8
             , Border.width 1
             , Border.rounded 12
             , Font.size 48
             , Font.color color
             , Font.variant Font.tabularNumbers
             , Element.focused []
             , Element.alpha opacity
             , style "transition" "opacity 1s linear"
             ]
                ++ buttonStyle
            )
            { onPress =
                if isEditable then
                    Just CycleWarmup

                else
                    Nothing
            , label = label
            }
        )


resetButton : Model -> Element Msg
resetButton model =
    let
        visible =
            model.timer.status == TimerPaused
    in
    Input.button
        [ centerX
        , paddingXY 24 12
        , Font.color colors.inactive
        , Element.alpha
            (if visible then
                1

             else
                0
            )
        , Element.focused []
        , style "transition" "opacity 0.3s"
        ]
        { onPress =
            if visible then
                Just Reset

            else
                Nothing
        , label = text "Reset"
        }


hideAfter : Int
hideAfter =
    10 * 1000


title : Model -> Element Msg
title model =
    let
        timer =
            model.timer

        sinceStart =
            case ( timer.startedAt, timer.now ) of
                ( Just started, Just now ) ->
                    Time.posixToMillis now - Time.posixToMillis started

                _ ->
                    0

        opacity =
            if model.timer.status /= TimerStarted || sinceStart < hideAfter then
                1

            else
                0
    in
    el
        [ centerX
        , Font.size 32
        , Font.semiBold
        , Element.alpha opacity
        , style "transition" "opacity 1s"
        ]
        (text "Meditation Timer")


toggles : Model -> Element Msg
toggles model =
    row [ centerX, spacingXY 32 0 ]
        [ toggleControl model model.config.intervals bellIcon ToggleIntervalBells
        , toggleControl model model.config.keepScreenOn phoneIcon ToggleScreenOn
        ]


center : Model -> Element Msg
center model =
    column
        [ centerX
        , spacing 24
        , padding 16
        ]
        [ warmupControl model
        , durationControl model
        , toggles model
        ]


footer : Model -> Element Msg
footer model =
    column
        [ centerX
        , spacing 16
        ]
        [ timerButton model
        , resetButton model
        ]


style : String -> String -> Attribute Msg
style name =
    Element.htmlAttribute << Html.Attributes.style name


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color colors.bg
        , Font.color colors.fg
        , Font.family [ Font.typeface "system-ui", Font.sansSerif ]
        ]
        (column
            [ Element.width Element.fill
            , Element.height Element.fill
            , style "padding-top" "calc(env(safe-area-inset-top) + 32px)"
            , style "padding-bottom" "calc(env(safe-area-inset-bottom) + 32px)"
            , style "padding-left" "env(safe-area-inset-left)"
            , style "padding-right" "env(safe-area-inset-right)"
            ]
            [ el [ centerX, alignTop ] (title model)
            , el [ centerX, centerY ] (center model)
            , el [ centerX, alignBottom ] (footer model)
            ]
        )
