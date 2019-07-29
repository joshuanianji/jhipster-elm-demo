module Account.PasswordResetRequest exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (requestResetPassword)
import Account.Common exposing (UiElement, sharedForms, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, fill, height, paddingXY, spacing, width)
import Element.Font as Font
import Form
import Form.View
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework exposing (flatMap, toElement, uiColumn)
import UiFramework.Alert as Alert
import UiFramework.Form
import UiFramework.Types exposing (Role(..))
import UiFramework.Typography exposing (h1)


type alias Model =
    Form.View.Model Values


type alias Values =
    { email : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | ResetRequest String
    | ResetResponse (WebData ())


init : ( Model, Cmd Msg )
init =
    ( Values "" |> Form.View.idle
    , Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    let
        translate =
            translator sharedState.language
    in
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        FormChanged newModel ->
            ( newModel
            , Cmd.none
            , NoUpdate
            )

        ResetRequest email ->
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , requestResetPassword email ResetResponse
                    , NoUpdate
                    )

        ResetResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.EmailNotFound

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        ResetResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.CheckEmail)
            )

        ResetResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Reset"
    , toElement (toContext sharedState) (content model)
    )


content : Model -> UiElement Msg
content model =
    uiColumn
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ h1
            [ paddingXY 0 30 ]
          <|
            tt AccountPhrases.ResetPasswordTitle
        , Alert.simple Warning <|
            tt AccountPhrases.ResetPasswordInfo
        , flatMap
            (\context ->
                let
                    fields =
                        sharedForms context
                in
                UiFramework.Form.layout
                    { onChange = FormChanged
                    , action = context.translate AccountPhrases.ResetButtonLabel
                    , loading = context.translate AccountPhrases.ResetButtonLoading
                    , validation = Form.View.ValidateOnSubmit
                    }
                    (Form.succeed ResetRequest
                        |> Form.append fields.emailField
                    )
                    model
            )
        ]
        |> wrapContent
