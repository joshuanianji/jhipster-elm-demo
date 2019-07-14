module Modules.Account.Register exposing (..)

import Api.Data.RegisterVM exposing(RegisterVM)
import Api.Request.Account exposing(registerAccount)
import Api.Request.Auth exposing(authenticationPost)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form exposing (Form)
import Form.View
import Http
import LocalStorage exposing (Event(..), jwtAuthenticationTokenKey)
import Modules.Account.I18n.Phrases as AccountPhrases
import Modules.Account.I18n.Translator exposing(translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import Validate exposing (Validator, ifBlank, validate)
import UiFramework.Form
import UiFramework.Typography exposing (h1)
import Utils


type alias Model = 
    Form.View.Model Values


type alias Values =
    { username : String
    , email : String
    , password : String
    , repeatPassword : String
    }


type Msg 
    = NavigateTo Route
    | FormChanged Model
    | Register String String String
    | RegisterResponse (WebData ())


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

        Register username email password ->
            let
                registerVM =
                    { username = Just username
                    , email = Just email
                    , password = Just password
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
                            translate AccountPhrases.RegistrationFailed -- TODO display error msg from server

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
            , NoUpdate
            )

        RegisterResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Registration"
    , el 
        [ width fill, height fill, centerX, paddingXY 100 10]
        ( content sharedState model )
    )


content : SharedState -> Model -> Element Msg
content sharedState model =
    let
        translate =
            translator sharedState.language
    in
    column
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [paddingXY 0 30]
            (text <| translate AccountPhrases.RegisterTitle)
        , UiFramework.Form.layout
            { onChange = FormChanged
            , action = translate AccountPhrases.RegisterButtonLabel
            , loading = translate AccountPhrases.RegisterButtonLoading
            , validation = Form.View.ValidateOnSubmit
            }
            (form sharedState)
            model
        ]


form : SharedState -> Form Values Msg
form sharedState =
    let
        translate =
            translator sharedState.language

        usernameField =
            Form.textField
                { parser = Ok
                , value = .username
                , update = \value values -> { values | username = value }
                , attributes =
                    { label = translate AccountPhrases.UsernameLabel
                    , placeholder = translate AccountPhrases.UsernamePlaceholder
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

        passwordField =
            Form.passwordField
                { parser = Ok
                , value = .password
                , update = \value values -> { values | password = value }
                , attributes =
                    { label = translate AccountPhrases.NewPasswordLabel
                    , placeholder = translate AccountPhrases.NewPasswordPlaceholder
                    }
                }

        repeatPasswordField =
            Form.meta
                (\values ->
                    Form.passwordField
                        { parser =
                            \value ->
                                if value == values.password then
                                    Ok ()

                                else
                                    Err <| translate AccountPhrases.PasswordNotMatch
                        , value = .repeatPassword
                        , update =
                            \newValue values_ ->
                                { values_ | repeatPassword = newValue }
                        , attributes =
                            { label = translate AccountPhrases.ConfirmPasswordLabel
                            , placeholder = translate AccountPhrases.ConfirmPasswordPlaceholder
                            }
                        }
                )
    in
    Form.succeed Register
        |> Form.append usernameField
        |> Form.append emailField
        |> Form.append 
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
            )

