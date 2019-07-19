module Modules.Account.PasswordUpdate exposing (Model, Msg(..), Values, content, form, init, update, view)

import Api.Request.Account exposing (updatePassword)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Form exposing (Form)
import Form.View
import Http
import Modules.Account.I18n.Phrases as AccountPhrases
import Modules.Account.I18n.Translator exposing (translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework.Form
import UiFramework.Padding
import UiFramework.Password
import UiFramework.Toasty
import UiFramework.Typography exposing (h1)
import Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    Form.View.Model Values


type alias Values =
    { currentPassword : String
    , password : String
    , repeatPassword : String
    }


type Msg
    = NavigateTo Route
    | FormChanged Model
    | ChangePassword String String
    | ChangePasswordResponse (WebData ())


init : ( Model, Cmd Msg )
init =
    ( Values "" "" "" |> Form.View.idle
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

        ChangePassword currentPassword newPassword ->
            let
                passwordUpdateVM =
                    { currentPassword = currentPassword
                    , newPassword = newPassword
                    }
            in
            case model.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    ( { model | state = Form.View.Loading }
                    , updatePassword sharedState.jwtToken passwordUpdateVM ChangePasswordResponse
                    , NoUpdate
                    )

        ChangePasswordResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.CannotUpdate

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | state = Form.View.Error errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        ChangePasswordResponse (RemoteData.Success ()) ->
            ( { model | state = Form.View.Idle }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.UpdateSuccess)
            )

        ChangePasswordResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Change Password"
    , el
        [ height fill, width fill, paddingXY 10 10 ]
        (content sharedState model)
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
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ h1 [ paddingXY 0 30 ]
            (text <| translate <| AccountPhrases.UpdatePasswordTitle (SharedState.getUsername sharedState))
        , UiFramework.Form.layout
            { onChange = FormChanged
            , action = translate AccountPhrases.SaveButtonLabel
            , loading = translate AccountPhrases.SaveButtonLoading
            , validation = Form.View.ValidateOnSubmit
            }
            (form sharedState)
            model
        ]
        |> UiFramework.Padding.responsive sharedState


form : SharedState -> Form Values Msg
form sharedState =
    let
        translate =
            translator sharedState.language

        currentPasswordField =
            Form.passwordField
                { parser = Ok
                , value = .currentPassword
                , update = \value values -> { values | currentPassword = value }
                , attributes =
                    { label = translate AccountPhrases.CurrentPasswordLabel
                    , placeholder = translate AccountPhrases.CurrentPasswordPlaceholder
                    }
                }

        passwordField =
            Form.passwordField
                { parser = UiFramework.Password.validate
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
    Form.succeed ChangePassword
        |> Form.append currentPasswordField
        |> Form.append
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
            )
