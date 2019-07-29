module Login.Logout exposing (Model, Msg(..), content, init, update, view)

import Element exposing (Element, fill, height, paddingXY, width)
import LocalStorage
import Login.Common exposing (UiElement, toContext, tt)
import Login.I18n.Phrases as LoginPhrases
import Shared.Constants exposing (jwtAuthenticationTokenKey)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework exposing (toElement, uiColumn)
import UiFramework.Alert as Alert
import UiFramework.Types exposing (Role(..))


type alias Model =
    {}


type Msg
    = NoOp


{-| When logout, we clear jwt token in local storage.
-}
init : ( Model, Cmd Msg )
init =
    ( {}, LocalStorage.clear jwtAuthenticationTokenKey )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState _ =
    ( "Logout"
    , toElement (toContext sharedState) content
    )


content : UiElement Msg
content =
    uiColumn
        [ width fill
        , height fill
        , paddingXY 30 10
        ]
        [ Alert.default
            |> Alert.withRole Success
            |> Alert.withLarge
            |> Alert.withChild (tt LoginPhrases.LogoutTitle)
            |> Alert.view
        ]
        |> wrapContent
