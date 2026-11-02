module Config exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Configuration =
    { duration : Int -- seconds
    , warmup : Int -- seconds
    , intervals : Bool
    , keepScreenOn : Bool
    }


defaultConfig : Configuration
defaultConfig =
    { duration = 600
    , warmup = 30
    , intervals = True
    , keepScreenOn = False
    }


minSessionDuration : Int
minSessionDuration =
    5 * 60


maxSessionDuration : Int
maxSessionDuration =
    60 * 60


warmupDurations : List Int
warmupDurations =
    [ 0, 30, 60, 120, 180, 300 ]


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


configDecoder : Decoder Configuration
configDecoder =
    let
        field name decoder default =
            Decode.oneOf [ Decode.field name decoder, Decode.succeed default ]
    in
    Decode.map4 Configuration
        (field "duration" (Decode.map (clamp minSessionDuration maxSessionDuration) Decode.int) defaultConfig.duration)
        (field "warmup" warmupDecoder defaultConfig.warmup)
        (field "intervals" Decode.bool defaultConfig.intervals)
        (field "keepScreenOn" Decode.bool defaultConfig.keepScreenOn)


encodeConfig : Configuration -> Encode.Value
encodeConfig config =
    Encode.object
        [ ( "duration", Encode.int config.duration )
        , ( "warmup", Encode.int config.warmup )
        , ( "intervals", Encode.bool config.intervals )
        , ( "keepScreenOn", Encode.bool config.keepScreenOn )
        ]


nextWarmup : Int -> Int
nextWarmup current =
    warmupDurations
        |> List.filter (\w -> w > current)
        |> List.head
        |> Maybe.withDefault 0
