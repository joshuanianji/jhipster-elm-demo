module Modules.Account.Settings exposing (Model, Msg(..), Values, content, form, init, update, view)

import Api.Data.Settings exposing (Settings)
import Api.Request.Account exposing (updateSettings)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form exposing (Form)
import Form.View
import Http
import I18n exposing (Language(..))
import Modules.Account.Common exposing(Context, UiElement, toContext, tt)
import LocalStorage exposing (Event(..), jwtAuthenticationTokenKey)
import Modules.Account.I18n.Phrases as AccountPhrases
import Modules.Account.I18n.Translator exposing (translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..), getUsername)
import Toasty.Defaults
import UiFramework exposing (WithContext, UiContextual, toElement, fromElement, uiText, uiRow, uiColumn, uiParagraph, flatMap)
import UiFramework.Form
import UiFramework.Padding
import UiFramework.Typography exposing (h1)
import Utils
import Validate exposing (Validator, ifBlank, validate)


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


init : ( Model, Cmd Msg )
init =
    ( Values "" "" "" "" |> Form.View.idle
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
                settings : Settings
                settings =
                    { username = SharedState.getUsername sharedState
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
    , toElement (toContext sharedState) (content (SharedState.getUsername sharedState) model)
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
        [ h1 [ paddingXY 0 30 ]
            <| tt <| AccountPhrases.SettingsTitle username
        , flatMap
            (\context ->
                UiFramework.Form.layout
                    { onChange = FormChanged
                    , action = context.translate AccountPhrases.SaveButtonLabel
                    , loading = context.translate AccountPhrases.SaveButtonLoading
                    , validation = Form.View.ValidateOnSubmit
                    }
                    (form context.language)
                    model
            )
        ]
        |> UiFramework.Padding.responsive


form : Language -> Form Values Msg
form language =
    let
        translate =
            translator language

        firstNameField =
            Form.textField
                { parser = Ok
                , value = .firstName
                , update = \value values -> { values | firstName = value }
                , attributes =
                    { label = translate AccountPhrases.FirstnameLabel
                    , placeholder = translate AccountPhrases.FirstnamePlaceholder
                    }
                }

        lastNameField =
            Form.textField
                { parser = Ok
                , value = .lastName
                , update = \value values -> { values | lastName = value }
                , attributes =
                    { label = translate AccountPhrases.LastnameLabel
                    , placeholder = translate AccountPhrases.LastnamePlaceholder
                    }
                }

        emailField =
            Form.textField
                { parser = Ok
                , value = .email
                , update = \value values -> { values | email = value }
                , attributes =
                    { label = translate AccountPhrases.EmailLabel
                    , placeholder = translate AccountPhrases.EmailPlaceholder
                    }
                }

        languageField =
            Form.selectField
                { parser = Ok
                , value = .languageKey
                , update = \value values -> { values | languageKey = value }
                , attributes =
                    { label = translate AccountPhrases.LanguageLabel
                    , placeholder = " - select language -"
                    , options =
                        List.map
                            (\lang -> ( I18n.languageCode lang, I18n.languageName lang ))
                            I18n.supportLanguages
                    }
                }
    in
    Form.succeed SaveSettings
        |> Form.append firstNameField
        |> Form.append lastNameField
        |> Form.append emailField
        |> Form.append languageField
