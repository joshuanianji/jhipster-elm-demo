module UiFramework.Internal exposing (..)

import Element exposing (Attribute, Color, Device, Element)
import Element.Lazy exposing (lazy)
import UiFramework.Types exposing (Role(..))


{-| An opaque type representing elm-ui Element with a context.
-}
type WithContext context msg
    = Node (context -> List (Element msg) -> Element msg) (List (WithContext context msg))
    | Leaf (context -> Element msg)


type alias UiContextual context =
    { context
        | device : Device
        , themeColor : Role -> Color
        , parentRole : Maybe Role
    }


{-| Convert to elm-ui Element.
-}
toElement : context -> WithContext context msg -> Element msg
toElement context wc =
    case wc of
        Node f children ->
            f context <| List.map (toElement context) children

        Leaf f ->
            lazy f context


fromElement : (context -> Element msg) -> WithContext context msg
fromElement =
    Leaf


flatMap : (c -> WithContext c msg) -> WithContext c msg
flatMap f =
    fromElement
        (\context ->
            toElement context (f context)
        )

{-| Custom node.
-}
node :
    (context -> List (Element msg) -> Element msg)
    -> List (WithContext context msg)
    -> WithContext context msg
node =
    Node


uiNone =
    Leaf <| \_ -> Element.none


uiText : (UiContextual c -> String) -> WithContext (UiContextual c) msg
uiText f =
    Leaf <| \context -> Element.text <| f context


uiRow : List (Attribute msg) -> List (WithContext c msg) -> WithContext c msg
uiRow attrs =
    node
        (\context ->
            Element.row attrs
         -- TODO adjust to column based on device class?
        )


uiColumn : List (Attribute msg) -> List (WithContext (UiContextual c) msg) -> WithContext (UiContextual c) msg
uiColumn attrs =
    node
        (\context ->
            Element.column attrs
        )


uiParagraph : List (Attribute msg) -> List (WithContext (UiContextual c) msg) -> WithContext (UiContextual c) msg
uiParagraph attrs =
    node
        (\context ->
            Element.paragraph attrs
        )