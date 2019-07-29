module Account.Register exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (registerAccount)
import Account.Common exposing (UiElement, sharedForms, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, alignLeft, fill, height, paddingXY, spacing, width)
import Form
import Form.View
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework exposing (flatMap, toElement, uiColumn)
import UiFramework.Form
import UiFramework.Typography exposing (h1)


type alias Model =
    Form.View.Model Values


type alias Values =
    { username : String
    , email : String
    , password : String
    , repeatPassword : String
    , languageKey : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | Register String String String String
    | RegisterResponse (WebData ())


init : ( Model, Cmd Msg )
init =
    ( Values "" "" "" "" "en" |> Form.View.idle
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
            ( newModel, Cmd.none, NoUpdate )

        Register username email password languageKey ->
            let
                registerVM =
                    { username = Just username
                    , email = Just email
                    , password = Just password
                    , languageKey = Just languageKey
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , registerAccount registerVM RegisterResponse
                    , NoUpdate
                    )

        RegisterResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.RegistrationFailed

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        RegisterResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.RegistrationSuccess)
            )

        RegisterResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Registration"
    , toElement (toContext sharedState) (content model)
    )


content : Model -> UiElement Msg
content model =
    uiColumn
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt AccountPhrases.RegisterTitle
        , flatMap
            (\context ->
                let
                    fields =
                        sharedForms context
                in
                UiFramework.Form.layout
                    { onChange = FormChanged
                    , action = context.translate AccountPhrases.RegisterButtonLabel
                    , loading = context.translate AccountPhrases.RegisterButtonLoading
                    , validation = Form.View.ValidateOnSubmit
                    }
                    (Form.succeed Register
                        |> Form.append fields.usernameField
                        |> Form.append fields.emailField
                        |> Form.append
                            (Form.succeed (\password _ -> password)
                                |> Form.append fields.passwordField
                                |> Form.append fields.repeatPasswordField
                            )
                        |> Form.append fields.languageField
                    )
                    model
            )
        ]
        |> wrapContent
