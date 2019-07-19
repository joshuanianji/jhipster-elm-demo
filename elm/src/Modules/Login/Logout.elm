module Modules.Login.Logout exposing (Model, Msg(..), content, init, update, view)

import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import LocalStorage exposing (jwtAuthenticationTokenKey)
import Modules.Login.Common exposing (Context, UiElement, toContext, tt)
import Modules.Login.I18n.Phrases as LoginPhrases
import Modules.Login.I18n.Translator exposing (translator)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework exposing (toElement, uiColumn)
import UiFramework.Alert as Alert
import UiFramework.Padding
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
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            translator sharedState.language
    in
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
        |> UiFramework.Padding.responsive
