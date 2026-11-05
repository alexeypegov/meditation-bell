module Ui exposing (render)

import Config exposing (maxSessionDuration, minSessionDuration)
import Element exposing (Attribute, Element, alignBottom, alpha, behindContent, centerX, centerY, clip, column, el, fill, focused, height, html, htmlAttribute, inFront, layout, maximum, mouseDown, onLeft, paddingXY, px, rgb255, rgba, row, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model, Msg(..))
import String
import Svg
import Svg.Attributes as SA
import Timer exposing (Phase(..), TimerStatus(..), currentElapsed, isOffSession, remaining)


colors :
    { bg : Element.Color
    , surface : Element.Color
    , fg : Element.Color
    , pressed : Element.Color
    , inactive : Element.Color
    , disabled : Element.Color
    }
colors =
    { bg = rgb255 20 20 20
    , surface = rgb255 50 50 50
    , fg = rgb255 240 240 240
    , pressed = rgb255 100 100 100
    , inactive = rgb255 100 100 100
    , disabled = rgb255 100 100 100
    }


{-| Step size during duration adjustments (secs).
-}
durationIncrementStep : Int
durationIncrementStep =
    5 * 60


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


icon : Bool -> List (Svg.Svg msg) -> Element msg
icon filled shapes =
    let
        paint =
            if filled then
                [ SA.fill "currentColor" ]

            else
                [ SA.fill "none", SA.stroke "currentColor", SA.strokeWidth "1.4", SA.strokeLinejoin "round" ]
    in
    html
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
    html
        (Svg.svg [ SA.viewBox "0 0 24 24", SA.width "26", SA.height "26" ]
            (if lit then
                [ frame, screen ]

             else
                [ frame ]
            )
        )


solidIcon : Int -> List (Svg.Svg msg) -> Element msg
solidIcon size shapes =
    html
        (Svg.svg
            [ SA.viewBox "0 0 24 24"
            , SA.width (String.fromInt size)
            , SA.height (String.fromInt size)
            , SA.style "display: block"
            ]
            [ Svg.g [ SA.fill "currentColor", SA.stroke "currentColor", SA.strokeWidth "1.5", SA.strokeLinejoin "round" ] shapes ]
        )


playIcon : Int -> Element msg
playIcon size =
    solidIcon size
        [ Svg.path
            [ SA.d "M9.5 6.5L18.5 12L9.5 17.5Z"
            , SA.transform "translate(-1 0)"
            ]
            []
        ]


pauseIcon : Int -> Element msg
pauseIcon size =
    solidIcon size
        [ Svg.rect [ SA.x "7", SA.y "6.5", SA.width "2.5", SA.height "11" ] []
        , Svg.rect [ SA.x "14.5", SA.y "6.5", SA.width "2.5", SA.height "11" ] []
        ]


rewindIcon : Int -> Element msg
rewindIcon size =
    solidIcon size
        [ Svg.path [ SA.d "M11.5 6.5L4 12L11.5 17.5Z", SA.transform "translate (-2 0)" ] []
        , Svg.path [ SA.d "M20 6.5L12.5 12L20 17.5Z", SA.transform "translate (-1 0)" ] []
        ]


plusIcon : Element msg
plusIcon =
    solidIcon 30
        [ Svg.rect [ SA.x "5.75", SA.y "11.45", SA.width "12.5", SA.height "1.1" ] []
        , Svg.rect [ SA.x "11.45", SA.y "5.75", SA.width "1.1", SA.height "12.5" ] []
        ]


minusIcon : Element msg
minusIcon =
    solidIcon 30
        [ Svg.rect [ SA.x "5.75", SA.y "11.45", SA.width "12.5", SA.height "1.1" ] [] ]


stepButton : Float -> Maybe Msg -> Element Msg -> Element Msg
stepButton opacity onPress iconEl =
    let
        pressed =
            case onPress of
                Just _ ->
                    [ mouseDown [ Background.color colors.pressed ] ]

                Nothing ->
                    []
    in
    Input.button
        ([ width (px 48)
         , height (px 48)
         , Border.rounded 24
         , Background.color colors.surface
         , Font.color colors.fg
         , alpha opacity
         , focused []
         , style "transition" "opacity 2s"
         ]
            ++ pressed
            ++ haptic (onPress /= Nothing)
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
            isOffSession model.timer.status

        step delta symbol =
            let
                target =
                    config.duration + delta
            in
            if not isEditable then
                stepButton 0 Nothing symbol

            else if target >= minSessionDuration && target <= maxSessionDuration then
                stepButton 1 (Just (ChangeDuration delta)) symbol

            else
                stepButton 0.3 Nothing symbol

        left =
            remaining Meditating model.timer (currentElapsed model.timer)

        color =
            if counting && model.timer.phase == Preparing then
                colors.disabled

            else
                colors.fg
    in
    row
        [ width fill
        , Font.size 72
        , Font.variant Font.tabularNumbers
        , Font.color color
        , Font.semiBold
        , style "transition" "color 2s"
        , style "justify-content" "space-evenly"
        ]
        [ step (negate durationIncrementStep) minusIcon
        , timeText model left
        , step durationIncrementStep plusIcon
        ]


timerButton : Model -> Element Msg
timerButton model =
    let
        running =
            model.timer.status == TimerStarted

        ( iconFor, msg ) =
            if running then
                ( pauseIcon, Pause )

            else
                ( playIcon, Start )
    in
    Input.button
        ([ width (px 96)
         , height (px 96)
         , Border.rounded 72
         , Background.color
            (if running then
                rgba 0 0 0 0

             else
                colors.surface
            )
         , Font.color colors.fg
         , style "transition" "background-color 2s, color 2s"
         , focused []
         ]
            ++ haptic True
        )
        { onPress = Just msg
        , label = el [ centerX, centerY ] (iconFor 72)
        }


toggleControl : Model -> Bool -> (Bool -> Element Msg) -> Msg -> Element Msg
toggleControl model on iconFor msg =
    let
        visible =
            isOffSession model.timer.status
    in
    Input.button
        ([ width (px 48)
         , height (px 48)
         , Border.rounded 24
         , Background.color colors.surface
         , mouseDown [ Background.color colors.pressed ]
         , Font.color colors.fg
         , alpha
            (if visible then
                1

             else
                0
            )
         , focused []
         , style "transition" "opacity 2s, color 2s"
         ]
            ++ haptic visible
        )
        { onPress =
            if visible then
                Just msg

            else
                Nothing
        , label = el [ centerX, centerY ] (iconFor on)
        }


timeText : Model -> Int -> Element Msg
timeText model ms =
    let
        formatted =
            if isOffSession model.timer.status then
                formatTime ms

            else
                formatCountdown ms
    in
    el [ style "min-width" "5ch" ]
        (el [ centerX ] (text formatted))


warmupControl : Model -> Element Msg
warmupControl model =
    let
        transparent =
            rgba 0 0 0 0

        timer =
            model.timer

        isEditable =
            isOffSession timer.status

        left =
            remaining Preparing timer (currentElapsed timer)

        label =
            if model.config.warmup == 0 then
                el [ style "min-width" "5ch" ] (el [ centerX ] (text (formatTime 0)))

            else if isEditable then
                timeText model (model.config.warmup * 1000)

            else
                timeText model left

        color =
            colors.fg

        opacity =
            if isEditable then
                1

            else
                1 - 1000 / toFloat (left + 1000)

        buttonStyle =
            if isEditable then
                [ Background.color colors.surface
                , mouseDown [ Background.color colors.pressed ]
                ]

            else
                [ Background.color transparent ]
    in
    el [ centerX ]
        (Input.button
            ([ centerX
             , paddingXY 0 8
             , Border.rounded 12
             , Font.size 48
             , Font.semiBold
             , Font.color color
             , Font.variant Font.tabularNumbers
             , focused []
             , alpha opacity
             , style "transition" "background 2s, opacity 2s"
             ]
                ++ buttonStyle
                ++ haptic isEditable
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
        ([ width (px 64)
         , height (px 64)
         , Border.rounded 36
         , Background.color colors.surface
         , Font.color colors.fg
         , mouseDown [ Background.color colors.pressed ]
         , alpha
            (if visible then
                1

             else
                0
            )
         , focused []
         , style "transition" "opacity 2s"
         ]
            ++ haptic visible
        )
        { onPress =
            if visible then
                Just Reset

            else
                Nothing
        , label = el [ centerX, centerY ] (rewindIcon 32)
        }


logo : Bool -> Element msg
logo hidden =
    let
        ringCount =
            10

        ring n r strokeWidth color =
            Svg.circle
                [ SA.cx "50"
                , SA.cy "15"
                , SA.r (String.fromFloat r)
                , SA.fill "none"
                , SA.strokeWidth (String.fromFloat strokeWidth)
                , SA.stroke color
                , SA.class
                    (if hidden then
                        "ring-out"

                     else
                        "ring-in"
                    )
                , SA.style
                    ("--o: "
                        ++ String.fromFloat ((1 - n / ringCount) ^ 2)
                        ++ "; --i: "
                        ++ String.fromInt ((round n - 1) // 2)
                    )
                ]
                []

        rings =
            List.range 1 ringCount
                |> List.filterMap
                    (\n ->
                        let
                            r i =
                                toFloat (n - i) ^ 2 + 3.0 * toFloat (n - i)

                            r0 =
                                r 1

                            r1 =
                                r 0

                            rc =
                                (r0 + r1) / 2

                            skip =
                                modBy 2 n == 0
                        in
                        if skip then
                            Nothing

                        else
                            Just (ring (toFloat n) rc (r1 - r0) "#777")
                    )
    in
    html
        (Svg.svg
            [ SA.viewBox "0 0 100 100"
            , SA.preserveAspectRatio "xMidYMin slice"
            , SA.width "100%"
            , SA.height "100%"
            , SA.style "display: block"
            ]
            rings
        )


title : Model -> Element Msg
title model =
    el
        [ width fill, height fill ]
        (logo (model.timer.phase == Meditating))


toggles : Model -> Element Msg
toggles model =
    row
        [ centerX
        , spacingXY 32 0
        ]
        [ toggleControl model model.config.intervals bellIcon ToggleIntervalBells
        , toggleControl model model.config.keepScreenOn phoneIcon ToggleScreenOn
        ]


controls : Model -> Element Msg
controls model =
    column
        [ width fill
        , centerX
        , spacingXY 0 32
        ]
        [ warmupControl model
        , durationControl model
        , toggles model
        , sessionControls model
        ]


sessionControls : Model -> Element Msg
sessionControls model =
    el
        [ centerX
        , paddingXY 0 32
        , onLeft (el [ centerX, centerY, paddingXY 32 0 ] (resetButton model))
        ]
        (timerButton model)


style : String -> String -> Attribute Msg
style name =
    htmlAttribute << Html.Attributes.style name


haptic : Bool -> List (Attribute Msg)
haptic enabled =
    if enabled then
        [ htmlAttribute (Html.Attributes.attribute "data-haptic" "")
        , inFront hapticSwitch
        ]

    else
        []


{-| iOS plays a haptic only for a real tap on a native switch, so an invisible one covers the button.
-}
hapticSwitch : Element msg
hapticSwitch =
    el [ width fill, height fill ]
        (html
            (Html.input
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.attribute "switch" ""
                , Html.Attributes.class "haptic-switch"
                , Html.Attributes.tabindex -1
                , Html.Attributes.attribute "aria-hidden" "true"
                ]
                []
            )
        )


render : Model -> Html Msg
render model =
    layout
        [ Background.color colors.bg
        , Font.color colors.fg
        , Font.family [ Font.typeface "Archivo", Font.typeface "system-ui", Font.sansSerif ]
        ]
        (el
            [ width fill
            , height fill
            , style "padding-top" "calc(env(safe-area-inset-top) + 32px)"
            , style "padding-bottom" "calc(env(safe-area-inset-bottom) + 32px)"
            , style "padding-left" "env(safe-area-inset-left)"
            , style "padding-right" "env(safe-area-inset-right)"
            , behindContent (title model)
            , clip
            ]
            (el
                [ width (fill |> maximum 600)
                , centerX
                , alignBottom
                ]
                (controls model)
            )
        )
