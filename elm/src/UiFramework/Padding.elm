module UiFramework.Padding exposing (responsive)

{-
   Responsive padding using fillPortion
-}

import Element exposing (..)
import UiFramework.Internal as Internal


type alias UiElement context msg =
    Internal.WithContext (Internal.UiContextual context) msg


{-| Elm-ui does not have a "percent padding". For example, I can't get 50% padding because I have to use pixels.

Thus, I am going to use fillPortion, which works a bit like bootstrap
Elm-ui will automatically designate a specified ratio of the width of each element with the fillPortion in a row or column.

Let's say I have (left, middle, right) as (1, 2, 1)
The Element.fillPortion will be 1 for the padding on the left and right,
and the Element.fillPortion will be 2 for the content

This means:

| -- left -- | -- middle (content) --| -- right -- |
Each "padding," which is just Element.none to display nothing, will be half the length of the content.

     If you want 0 padding, just put 0 in the respective position

-}
responsive : UiElement context msg -> UiElement context msg
responsive content =
    Internal.fromElement
        (\context ->
            let
                ( left, middle, right ) =
                    case context.device.class of
                        BigDesktop ->
                            ( 1, 1, 1 )

                        Desktop ->
                            ( 1, 2, 1 )

                        Tablet ->
                            case context.device.orientation of
                                Portrait ->
                                    ( 1, 8, 1 )

                                Landscape ->
                                    ( 1, 4, 1 )

                        Phone ->
                            ( 0, 1, 0 )
            in
            -- basically squish the content between two Element.none
            row
                [ width fill ]
                [ el [ width <| fillPortion left ] none
                , el [ width <| fillPortion middle ] (Internal.toElement context content)
                , el [ width <| fillPortion right ] none
                ]
        )
