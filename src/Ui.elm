module Ui exposing (render)

import Config exposing (maxSessionDuration, minSessionDuration)
import Element exposing (Attribute, Element, centerX, centerY, column, el, padding, paddingXY, row, spacing, spacingXY, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Model exposing (Model, Msg(..))
import Svg
import Svg.Attributes as SA
import Theme exposing (colors, eldritch)
import Time
import Timer exposing (Phase(..), TimerStatus(..), currentElapsed, isOffSession, remaining)


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


solidIcon : Int -> List (Svg.Svg msg) -> Element msg
solidIcon size shapes =
    Element.html
        (Svg.svg [ SA.viewBox "0 0 24 24", SA.width (String.fromInt size), SA.height (String.fromInt size) ]
            [ Svg.g [ SA.fill "currentColor", SA.stroke "currentColor", SA.strokeWidth "1.5", SA.strokeLinejoin "round" ] shapes ]
        )


playIcon : Int -> Element msg
playIcon size =
    solidIcon size [ Svg.path [ SA.d "M9.5 6.5L18.5 12L9.5 17.5Z" ] [] ]


pauseIcon : Int -> Element msg
pauseIcon size =
    solidIcon size
        [ Svg.rect [ SA.x "7", SA.y "6.5", SA.width "2.5", SA.height "11" ] []
        , Svg.rect [ SA.x "14.5", SA.y "6.5", SA.width "2.5", SA.height "11" ] []
        ]


rewindIcon : Int -> Element msg
rewindIcon size =
    solidIcon size
        [ Svg.path [ SA.d "M11.5 6.5L4 12L11.5 17.5Z" ] []
        , Svg.path [ SA.d "M20 6.5L12.5 12L20 17.5Z" ] []
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
        , Element.onLeft (side (step (negate durationIncrementStep) minusIcon))
        , Element.onRight (side (step durationIncrementStep plusIcon))
        ]
        (el
            [ Font.size 72
            , Font.variant Font.tabularNumbers
            , Font.color color
            , Font.semiBold
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

        ( iconFor, msg ) =
            if running then
                ( pauseIcon, Pause )

            else
                ( playIcon, Start )
    in
    Input.button
        ([ Element.width (Element.px 96)
         , Element.height (Element.px 96)
         , Border.rounded 72
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
         , style "transition" "background-color 0.3s, color 0.3s"
         , Element.focused []
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
        ([ Element.width (Element.px 48)
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
    text formatted


warmupControl : Model -> Element Msg
warmupControl model =
    let
        transparent =
            Element.rgba 0 0 0 0

        timer =
            model.timer

        isEditable =
            isOffSession timer.status

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
             , Font.medium
             , Font.color color
             , Font.variant Font.tabularNumbers
             , Element.focused []
             , Element.alpha opacity
             , style "transition" "opacity 1s linear"
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
        ([ Element.width (Element.px 64)
         , Element.height (Element.px 64)
         , Border.rounded 36
         , Background.color colors.surface
         , Element.mouseDown [ Background.color eldritch.tidalSurface ]
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
            ++ haptic visible
        )
        { onPress =
            if visible then
                Just Reset

            else
                Nothing
        , label = el [ centerX, centerY ] (rewindIcon 32)
        }


hideAfter : Int
hideAfter =
    10 * 1000


logo : Element msg
logo =
    let
        ring r width color =
            Svg.circle
                [ SA.cx "256"
                , SA.cy "256"
                , SA.r r
                , SA.fill "none"
                , SA.stroke color
                , SA.strokeWidth width
                ]
                []
    in
    Element.html
        (Svg.svg
            [ SA.viewBox "64 64 384 384" -- the outer ring's edge, no margin
            , SA.width "128"
            , SA.height "128"
            , SA.style "display: block" -- inline svg leaves a gap under it for text descenders
            ]
            [ ring "164.5" "55" "#3D4256" -- outer ring
            , ring "82.5" "27" "#596173" -- middle ring
            , Svg.circle [ SA.cx "256", SA.cy "256", SA.r "18", SA.fill "#A9B4BA" ] []
            ]
        )


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
        , Element.alpha opacity
        , style "transition" "opacity 1s"
        ]
        logo


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
        , spacing 48
        , padding 16
        ]
        [ warmupControl model
        , durationControl model
        , toggles model
        ]


footer : Model -> Element Msg
footer model =
    el
        [ centerX
        , Element.onLeft (el [ centerX, centerY, paddingXY 32 0 ] (resetButton model))
        ]
        (timerButton model)


style : String -> String -> Attribute Msg
style name =
    Element.htmlAttribute << Html.Attributes.style name


part : Element Msg -> Element Msg
part content =
    el [ Element.width Element.fill, Element.height Element.fill ]
        (el [ centerX, centerY ] content)


haptic : Bool -> List (Attribute Msg)
haptic enabled =
    if enabled then
        [ Element.htmlAttribute (Html.Attributes.attribute "data-haptic" "")
        , Element.inFront hapticSwitch
        ]

    else
        []


{-| iOS plays a haptic only for a real tap on a native switch, so an invisible one covers the button.
-}
hapticSwitch : Element msg
hapticSwitch =
    el [ Element.width Element.fill, Element.height Element.fill ]
        (Element.html
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
    Element.layout
        [ Background.color colors.bg
        , Font.color colors.fg
        , Font.family [ Font.typeface "Archivo", Font.typeface "system-ui", Font.sansSerif ]
        ]
        (column
            [ Element.width Element.fill
            , Element.height Element.fill
            , style "padding-top" "calc(env(safe-area-inset-top) + 32px)"
            , style "padding-bottom" "calc(env(safe-area-inset-bottom) + 32px)"
            , style "padding-left" "env(safe-area-inset-left)"
            , style "padding-right" "env(safe-area-inset-right)"
            ]
            [ part (title model)
            , el [ centerX ] (center model)
            , part (footer model)
            ]
        )
