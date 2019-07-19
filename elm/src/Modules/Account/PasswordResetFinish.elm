module Modules.Account.PasswordResetFinish exposing (Model, Msg(..), Values, content, form, init, update, view)

import Api.Data.KeyAndPasswordVM exposing(KeyAndPasswordVM)
import Api.Request.Account exposing (resetPassword)
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
import Modules.Account.I18n.Phrases as AccountPhrases
import Modules.Account.I18n.Translator exposing(translator)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework exposing (WithContext, UiContextual, toElement, fromElement, uiText, uiRow, uiColumn, uiParagraph, flatMap)
import UiFramework.Alert as Alert
import UiFramework.Form
import UiFramework.Toasty
import UiFramework.Types exposing (Role(..))
import UiFramework.Typography exposing (h1)
import Utils
import Validate exposing (Validator, ifBlank, validate)


type alias Model =
    { key : Maybe String
    , resetForm : Form.View.Model Values
    }


type alias Values =
    { password : String
    , repeatPassword : String
    }


type Msg
    = NavigateTo Route
    | FormChanged (Form.View.Model Values)
    | ResetPassword String
    | ResetResponse (WebData ())


init : Maybe String -> ( Model, Cmd Msg )
init key =
    ( { key = key
      , resetForm = Values "" "" |> Form.View.idle
      }
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

        FormChanged newFormModel ->
            ( { model | resetForm = newFormModel }
            , Cmd.none
            , NoUpdate
            )

        ResetPassword password ->
            case model.resetForm.state of
                Form.View.Loading ->
                    ( model, Cmd.none, NoUpdate )

                _ ->
                    let
                        oldResetForm = model.resetForm

                        newResetForm = { oldResetForm | state = Form.View.Loading }

                        keyAndPasswordVM =
                            { key = model.key
                            , newPassword = Just password
                            }
                    in
                    ( { model | resetForm = newResetForm }
                    , resetPassword keyAndPasswordVM ResetResponse
                    , NoUpdate
                    )

        ResetResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.CannotReset

                        _ ->
                            translate AccountPhrases.ServerError
            in
            let
                oldResetForm = model.resetForm

                newResetForm = { oldResetForm | state = Form.View.Error errorString }
            in
            
            ( { model | resetForm = newResetForm }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        ResetResponse (RemoteData.Success ()) ->
            let
                oldResetForm = model.resetForm

                newResetForm = { oldResetForm | state = Form.View.Idle }
            in
            ( { model |  resetForm = newResetForm }
            , Utils.perform <| NavigateTo Login
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.ResetSuccess)
            )

        ResetResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    let
        translate =
            translator sharedState.language
    in
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
            <| tt AccountPhrases.ResetPasswordTitle
        , case model.key of
            Nothing ->
                Alert.simple Danger <|
                    tt AccountPhrases.MissingResetKey
            Just k ->
                flatMap
                    (\context ->
                        UiFramework.Form.layout
                            { onChange = FormChanged
                            , action = context.translate AccountPhrases.ResetButtonLabel
                            , loading = context.translate AccountPhrases.ResetButtonLoading
                            , validation = Form.View.ValidateOnSubmit
                            }
                            (form context.language)
                            model.resetForm
                    )
        ]


form : Language -> Form Values Msg
form language =
    let
        translate =
            translator language

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
    Form.succeed ResetPassword
        |> Form.append
            (Form.succeed (\password _ -> password)
                |> Form.append passwordField
                |> Form.append repeatPasswordField
            )
