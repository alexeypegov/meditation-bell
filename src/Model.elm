module Model exposing (..)

import Config exposing (Configuration)
import Time
import Timer exposing (TimerState)


type alias Model =
    { timer : TimerState
    , config : Configuration
    }


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
