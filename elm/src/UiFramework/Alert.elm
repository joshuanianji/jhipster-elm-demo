module UiFramework.Alert exposing
    ( Alert
    , Options
    , default
    , link
    , simple
    , view
    , withChild
    , withExtraAttrs
    , withLarge
    , withRole
    , withSmall
    )

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import UiFramework.Colors exposing (..)
import UiFramework.Internal as Internal
import UiFramework.Types exposing (Role(..), ScreenSize(..), getFontSize)

type alias UiElement context msg =
    Internal.WithContext (Internal.UiContextual context) msg


type Alert context msg
    = Alert (Options context msg)


type alias Options context msg =
    { role : Role
    , size : ScreenSize
    , attributes : List (Attribute msg)
    , child : UiElement context msg
    }


withRole : Role -> Alert context msg -> Alert context msg
withRole role (Alert options) =
    Alert { options | role = role }


withLarge : Alert context msg -> Alert context msg
withLarge (Alert options) =
    Alert { options | size = LG }


withSmall : Alert context msg -> Alert context msg
withSmall (Alert options) =
    Alert { options | size = SM }


withExtraAttrs : List (Attribute msg) -> Alert context msg -> Alert context msg
withExtraAttrs attributes (Alert options) =
    Alert { options | attributes = attributes }


withChild : UiElement context msg -> Alert context msg -> Alert context msg
withChild child (Alert options) =
    Alert { options | child = child }


defaultOptions : Options context msg
defaultOptions =
    { role = Primary
    , size = MD
    , attributes = []
    , child = Internal.fromElement (\context -> none)
    }


default : Alert context msg
default =
    Alert defaultOptions


simple : Role -> UiElement context msg -> UiElement context msg
simple role child =
    default
        |> withRole role
        |> withChild child
        |> view


-- Rendering Alert


view : Alert context msg -> UiElement context msg
view (Alert options) =
    Internal.fromElement
        (\context ->
            el (viewAttributes context options) <|
                Internal.toElement
                    { context | parentRole = Just options.role } options.child
        )


viewAttributes : (Internal.UiContextual context) -> Options context mag -> List (Attribute msg)
viewAttributes context options =
    let
        backgroundColor =
            alertBackgroundColor context.themeColor options.role

        borderColor =
            alertBorderColor context.themeColor options.role

        fontColor =
            alertFontColor context.themeColor options.role

        fontSize =
            getFontSize options.size
    in
    [ width fill
    , paddingXY 20 16
    , Font.alignLeft
    , Font.size fontSize
    , Font.color fontColor
    , Border.rounded 4
    , Border.width 1
    , Border.solid
    , Border.color borderColor
    , Background.color backgroundColor
    ]


-- Alert Link
link :
    { onPress : Maybe msg
    , label : UiElement context msg
    }
    -> UiElement context msg
link { onPress, label } =
    Internal.fromElement
        (\context ->
            let
                role = context.parentRole |> Maybe.withDefault Primary

                fontColor =
                    alertLinkFontColor context.themeColor role
            in
            Input.button
                [ Font.bold, Font.color fontColor ]
                { onPress = onPress
                , label = Internal.toElement context label
                }
        )
