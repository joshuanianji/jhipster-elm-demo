module Account.Settings exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (updateSettings)
import Account.Common exposing (UiElement, sharedForms, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, alignLeft, fill, height, paddingXY, spacing, width)
import Form
import Form.View
import Http
import LocalStorage exposing (Event(..))
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import Shared.Api.User exposing (User)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..), getUsername)
import Toasty.Defaults
import UiFramework exposing (flatMap, toElement, uiColumn)
import UiFramework.Form
import UiFramework.Typography exposing (h1)


type alias Model =
    Form.View.Model Values


type alias Values =
    { firstName : String
    , lastName : String
    , email : String
    , languageKey : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | SaveSettings String String String String
    | SaveSettingsResponse (WebData ())


init : User -> ( Model, Cmd Msg )
init user =
    ( Values
        (user.firstName |> Maybe.withDefault "")
        (user.lastName |> Maybe.withDefault "")
        user.email
        user.languageKey
        |> Form.View.idle
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

        SaveSettings firstName lastName email languageKey ->
            let
                settings =
                    { username = getUsername sharedState
                    , firstName = firstName
                    , lastName = lastName
                    , email = email
                    , languageKey = languageKey
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , updateSettings sharedState.jwtToken settings SaveSettingsResponse
                    , NoUpdate
                    )

        SaveSettingsResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.CannotSaveSettings

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        SaveSettingsResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.SaveSuccess)
            )

        SaveSettingsResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Settings"
    , toElement (toContext sharedState) (content (getUsername sharedState) model)
    )


content : String -> Model -> UiElement Msg
content username model =
    uiColumn
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt <|
                AccountPhrases.SettingsTitle username
        , flatMap
            (\context ->
                let
                    fields =
                        sharedForms context
                in
                UiFramework.Form.layout
                    { onChange = FormChanged
                    , action = context.translate AccountPhrases.SaveButtonLabel
                    , loading = context.translate AccountPhrases.SaveButtonLoading
                    , validation = Form.View.ValidateOnSubmit
                    }
                    (Form.succeed SaveSettings
                        |> Form.append fields.firstNameField
                        |> Form.append fields.lastNameField
                        |> Form.append fields.emailField
                        |> Form.append fields.languageField
                    )
                    model
            )
        ]
        |> wrapContent
