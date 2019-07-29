module Error.NotFound exposing (Model, Msg(..), content, init, update, view)

import Element exposing (Element, centerX, fill, height, paddingXY)
import Element.Font as Font
import Error.Common exposing (UiElement, toContext, tt)
import Error.I18n.Phrases as ErrorPhrases
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework exposing (toElement, uiColumn)
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
view sharedState _ =
    ( " 404 Not Found"
    , toElement (toContext sharedState) content
    )


content : UiElement Msg
content =
    uiColumn
        [ height fill, centerX, paddingXY 10 10, Font.center ]
        [ h1 [] <| tt ErrorPhrases.NotFoundTitle
        ]
