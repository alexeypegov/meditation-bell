module Bells exposing (..)

import Random
import Timer exposing (TimerState, totalMillis)


type Bell
    = Starting
    | Interval Int
    | Final


{-| Number of interval sounds, as public/sounds/interval-1.mp3 ... interval-N.mp3
-}
intervalBellVariantCount : Int
intervalBellVariantCount =
    3


{-| Maximum number of interval bells
-}
maxBells : Int
maxBells =
    7


{-| Tries per bell before it's dropped; the first bell gets many, so a session
always has at least one interval bell when one can fit.
-}
bellScheduleTries : Int
bellScheduleTries =
    3


firstScheduleTries : Int
firstScheduleTries =
    100


{-| Minimal interval between bells
-}
minBellGap : Int
minBellGap =
    60 * 1000


intervalBellCount : TimerState -> Random.Generator Int
intervalBellCount timer =
    if timer.intervals then
        let
            -- minutes =
            --     toFloat timer.duration / 60000
            -- clamp 1 maxBells (round (1.5 * sqrt (minutes / 10)))
            least =
                1
        in
        Random.int least maxBells

    else
        Random.constant 0


{-| Places `left` bells at random between `start` and `end`, none closer than
`minGap` to any bell in `taken` (which starts with the start and final bells).
A bell that can't find room within its tries is dropped.
-}
placeNextBell : Int -> Int -> List Int -> Int -> Int -> Random.Generator (List Int)
placeNextBell left tries taken start end =
    if left <= 0 then
        Random.constant taken

    else
        Random.int (start // 1000) (end // 1000)
            |> Random.map ((*) 1000)
            |> Random.andThen
                (\t ->
                    if List.all (\b -> abs (t - b) >= minBellGap) taken then
                        placeNextBell (left - 1) bellScheduleTries (t :: taken) start end

                    else if tries > 1 then
                        placeNextBell left (tries - 1) taken start end

                    else
                        placeNextBell (left - 1) bellScheduleTries taken start end
                )


planBells : TimerState -> TimerState
planBells timer =
    let
        start =
            timer.warmup

        end =
            totalMillis timer

        plan =
            intervalBellCount timer
                |> Random.andThen (\count -> placeNextBell count firstScheduleTries [ start, end ] start end)
                |> Random.map (List.filter (\t -> t /= start && t /= end) >> List.sort)

        ( bells, seed ) =
            Random.step plan timer.seed
    in
    { timer | bells = bells, seed = seed }
