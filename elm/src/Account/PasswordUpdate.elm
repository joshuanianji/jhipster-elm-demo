module Account.PasswordUpdate exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (updatePassword)
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
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..), getUsername)
import Toasty.Defaults
import UiFramework exposing (flatMap, toElement, uiColumn)
import UiFramework.Form
import UiFramework.Typography exposing (h1)


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
    , toElement (toContext sharedState) (content (getUsername sharedState) model)
    )


content : String -> Model -> UiElement Msg
content username model =
    uiColumn
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        , Font.alignLeft
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt <|
                AccountPhrases.UpdatePasswordTitle username
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
                    (Form.succeed ChangePassword
                        |> Form.append fields.currentPasswordField
                        |> Form.append
                            (Form.succeed (\password _ -> password)
                                |> Form.append fields.passwordField
                                |> Form.append fields.repeatPasswordField
                            )
                    )
                    model
            )
        ]
        |> wrapContent
