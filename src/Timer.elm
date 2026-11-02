module Timer exposing (..)

import Config exposing (Configuration)
import Random
import Time


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


isOffSession : TimerStatus -> Bool
isOffSession status =
    status == TimerReady || status == TimerFinished


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


currentElapsed : TimerState -> Int
currentElapsed timer =
    case timer.now of
        Just now ->
            elapsedMillis timer now

        Nothing ->
            timer.elapsedBefore
