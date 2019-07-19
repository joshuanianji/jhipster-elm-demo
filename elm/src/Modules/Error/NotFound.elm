module Modules.Error.NotFound exposing (Model, Msg(..), content, init, subscriptions, update, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Modules.Error.Common exposing (Context, UiElement, toContext, tt)
import Modules.Error.I18n.Phrases as ErrorPhrases
import Modules.Error.I18n.Translator exposing (translator)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework exposing (UiContextual, WithContext, flatMap, fromElement, toElement, uiColumn, uiParagraph, uiRow, uiText)
import UiFramework.Typography exposing (h1)


type alias Model =
    {}


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            translator sharedState.language
    in
    ( " 404 Not Found"
    , toElement (toContext sharedState) content
    )


content =
    uiColumn
        [ height fill, centerX, paddingXY 10 10, Font.center ]
        [ h1 [] <| tt ErrorPhrases.NotFoundTitle
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
