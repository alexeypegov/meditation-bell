module Main exposing (main)

import Bells exposing (Bell(..), intervalBellVariantCount, minBellGap, planBells)
import Browser
import Config exposing (Configuration, configDecoder, defaultConfig, encodeConfig, maxSessionDuration, minSessionDuration, nextWarmup)
import Html exposing (Html)
import Json.Decode as Decode
import Model exposing (Model, Msg(..))
import Ports exposing (keepAwake, playBell, saveSettings, stopBells)
import Process
import Random
import Task
import Time
import Timer exposing (Phase(..), TimerStatus(..), elapsedMillis, freshTimer, isOffSession, totalMillis)
import Ui exposing (render)


main : Program (Maybe String) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Maybe String -> ( Model, Cmd Msg )
init stored =
    let
        config =
            stored
                |> Maybe.andThen (Decode.decodeString configDecoder >> Result.toMaybe)
                |> Maybe.withDefault defaultConfig
    in
    ( { timer = freshTimer config
      , config = config
      }
    , Cmd.none
    )



-- CONFIG


setConfig : Configuration -> Model -> ( Model, Cmd Msg )
setConfig config model =
    ( { model | config = config, timer = freshTimer config }
    , saveSettings (encodeConfig config)
    )



-- UPDATE


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
                        , bells = List.filter (\at -> at >= elapsed + minBellGap) base.bells
                    }

                ( advanced, bellCmd ) =
                    advance now { model | timer = started }
            in
            ( advanced, Cmd.batch [ setKeepAwake True model, bellCmd, exactTicks elapsed ] )

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
                , Cmd.batch [ setKeepAwake False model, stopBells () ]
                )

            else
                ( model, Cmd.none )

        Reset ->
            ( { model | timer = freshTimer model.config }
            , Cmd.batch [ setKeepAwake False model, stopBells () ]
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
            if isOffSession timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | duration = clamp minSessionDuration maxSessionDuration (config.duration + step) } model

            else
                ( model, Cmd.none )

        CycleWarmup ->
            if isOffSession timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | warmup = nextWarmup config.warmup } model

            else
                ( model, Cmd.none )

        ToggleScreenOn ->
            if isOffSession timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | keepScreenOn = not config.keepScreenOn } model

            else
                ( model, Cmd.none )

        ToggleIntervalBells ->
            if isOffSession timer.status then
                let
                    config =
                        model.config
                in
                setConfig { config | intervals = not config.intervals } model

            else
                ( model, Cmd.none )


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
        , Cmd.batch [ setKeepAwake False model, ringBell Final ]
        )

    else if timer.phase == Preparing && elapsed >= timer.warmup then
        let
            next =
                planBells { timer | phase = Meditating, now = Just now }
        in
        ( { model | timer = next }
        , ringBell Starting
        )

    else if isBellDue timer.bells elapsed then
        let
            ( sound, seed ) =
                Random.step (Random.int 1 intervalBellVariantCount) timer.seed

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


setKeepAwake : Bool -> Model -> Cmd msg
setKeepAwake on model =
    keepAwake { session = on, screen = on && model.config.keepScreenOn }



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


isBellDue : List Int -> Int -> Bool
isBellDue bells elapsed =
    case bells of
        at :: _ ->
            elapsed >= at

        [] ->
            False


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



-- VIEW


view : Model -> Html Msg
view model =
    render model
