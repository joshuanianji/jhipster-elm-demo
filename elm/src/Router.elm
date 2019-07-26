module Router exposing (DropdownMenuState(..), Model, Msg(..), Page(..), init, initWith, subscriptions, update)

import Browser.Events as Events
import Browser.Navigation
import I18n
import Json.Decode as Json
import LocalStorage exposing (Event(..), jwtAuthenticationTokenKey)
import Modules.Account.Activate as Activate
import Modules.Account.PasswordResetFinish as PasswordResetFinish
import Modules.Account.PasswordResetRequest as PasswordResetRequest
import Modules.Account.PasswordUpdate as PasswordUpdate
import Modules.Account.Register as Register
import Modules.Account.Settings as Settings
import Modules.Error.NotFound as NotFound
import Modules.Home.Home as Home
import Modules.Login.Login as Login
import Modules.Login.Logout as Logout
import Modules.Shared.Api.Request exposing (getCurrentAccount)
import Modules.Shared.Api.User exposing (User)
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), fromUrl, routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Task
import Toasty
import Toasty.Defaults
import UiFramework.Configuration exposing (ThemeConfig)
import Url exposing (Url)
import Utils


type alias Model =
    { currentPage : Page
    , route : Route
    , toggleMenuState : Bool
    , dropdownMenuState : DropdownMenuState
    , loginDialogState : Bool
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    }


type DropdownMenuState
    = AllClosed
    | AccountOpen
    | LanguageOpen
    | ThemeOpen


type Page
    = NotFoundPage NotFound.Model
    | HomePage Home.Model
    | LoginPage Login.Model
    | LogoutPage Logout.Model
    | RegisterPage Register.Model
    | PasswordResetRequestPage PasswordResetRequest.Model
    | PasswordResetFinishPage PasswordResetFinish.Model
    | SettingsPage Settings.Model
    | PasswordUpdatePage PasswordUpdate.Model
    | ActivatePage Activate.Model


type Msg
    = UrlChanged Url
    | NavigateTo Route
    | GetAccountResponse (WebData User)
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | ToggleMenu
    | ToggleAccountDropdown
    | ToggleLanguageDropdown
    | ToggleThemeDropdown
    | CloseDropdown
    | SelectLanguage I18n.Language
    | SelectTheme ThemeConfig
    | HomeMsg Home.Msg
    | LoginMsg Login.Msg
    | LogoutMsg Logout.Msg
    | RegisterMsg Register.Msg
    | NotFoundMsg NotFound.Msg
    | PasswordResetRequestMsg PasswordResetRequest.Msg
    | PasswordResetFinishMsg PasswordResetFinish.Msg
    | SettingsMsg Settings.Msg
    | PasswordUpdateMsg PasswordUpdate.Msg
    | ActivateMsg Activate.Msg
    | NoOp


init : Url -> ( Model, Cmd Msg )
init url =
    let
        currentRoute =
            fromUrl url
    in
    ( { currentPage = NotFoundPage {}
      , route = currentRoute
      , toggleMenuState = False
      , dropdownMenuState = AllClosed
      , loginDialogState = False
      , toasties = Toasty.initialState
      }
    , (Task.perform identity << Task.succeed) <| UrlChanged url
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.dropdownMenuState /= AllClosed then
        Events.onClick (Json.succeed CloseDropdown)

    else
        Sub.none


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case ( msg, model.currentPage ) of
        ( UrlChanged location, _ ) ->
            let
                route =
                    fromUrl location

                ( newModel, newCmd, newSharedStateUpdate ) =
                    navigateTo route model
            in
            ( { newModel | route = route }
            , newCmd
            , newSharedStateUpdate
            )

        ( NavigateTo route, _ ) ->
            ( { model | dropdownMenuState = AllClosed }
            , Browser.Navigation.pushUrl sharedState.navKey (routeToUrlString route)
            , NoUpdate
            )

        ( GetAccountResponse (RemoteData.Failure _), _ ) ->
            ( model
            , Cmd.none
            , NoUpdate
            )

        ( GetAccountResponse (RemoteData.Success user), _ ) ->
            ( model
            , Cmd.none
            , UpdateUser user
            )

        ( GetAccountResponse _, _ ) ->
            ( model, Cmd.none, NoUpdate )

        ( ToggleMenu, _ ) ->
            ( { model | toggleMenuState = not model.toggleMenuState }
            , Cmd.none
            , NoUpdate
            )

        ( ToggleLanguageDropdown, _ ) ->
            let
                dropdownMenuState =
                    if model.dropdownMenuState == LanguageOpen then
                        AllClosed

                    else
                        LanguageOpen
            in
            ( { model | dropdownMenuState = dropdownMenuState }
            , Cmd.none
            , NoUpdate
            )

        ( ToggleThemeDropdown, _ ) ->
            let
                dropdownMenuState =
                    if model.dropdownMenuState == ThemeOpen then
                        AllClosed

                    else
                        ThemeOpen
            in
            ( { model | dropdownMenuState = dropdownMenuState }
            , Cmd.none
            , NoUpdate
            )

        ( ToggleAccountDropdown, _ ) ->
            let
                dropdownMenuState =
                    if model.dropdownMenuState == AccountOpen then
                        AllClosed

                    else
                        AccountOpen
            in
            ( { model | dropdownMenuState = dropdownMenuState }
            , Cmd.none
            , NoUpdate
            )

        ( CloseDropdown, _ ) ->
            ( { model | dropdownMenuState = AllClosed }
            , Cmd.none
            , NoUpdate
            )

        ( SelectLanguage language, _ ) ->
            ( { model | dropdownMenuState = AllClosed }
            , Cmd.none
            , UpdateLanguage language
            )

        ( SelectTheme themeConfig, _ ) ->
            ( { model | dropdownMenuState = AllClosed }
            , Cmd.none
            , UpdateTheme themeConfig
            )

        ( ToastyMsg subMsg, _ ) ->
            Toasty.update Toasty.Defaults.config ToastyMsg subMsg model
                |> Utils.flip Utils.tupleExtend NoUpdate

        ( HomeMsg subMsg, HomePage subModel ) ->
            Home.update sharedState subMsg subModel
                |> updateWith HomePage HomeMsg model

        ( LoginMsg subMsg, LoginPage subModel ) ->
            Login.update sharedState subMsg subModel
                |> updateWith LoginPage LoginMsg model

        ( LogoutMsg subMsg, LogoutPage subModel ) ->
            Logout.update sharedState subMsg subModel
                |> updateWith LogoutPage LogoutMsg model

        ( RegisterMsg subMsg, RegisterPage subModel ) ->
            Register.update sharedState subMsg subModel
                |> updateWith RegisterPage RegisterMsg model

        ( PasswordResetRequestMsg subMsg, PasswordResetRequestPage subModel ) ->
            PasswordResetRequest.update sharedState subMsg subModel
                |> updateWith PasswordResetRequestPage PasswordResetRequestMsg model

        ( PasswordResetFinishMsg subMsg, PasswordResetFinishPage subModel ) ->
            PasswordResetFinish.update sharedState subMsg subModel
                |> updateWith PasswordResetFinishPage PasswordResetFinishMsg model

        ( SettingsMsg subMsg, SettingsPage subModel ) ->
            Settings.update sharedState subMsg subModel
                |> updateWith SettingsPage SettingsMsg model

        ( PasswordUpdateMsg subMsg, PasswordUpdatePage subModel ) ->
            PasswordUpdate.update sharedState subMsg subModel
                |> updateWith PasswordUpdatePage PasswordUpdateMsg model

        ( ActivateMsg subMsg, ActivatePage subModel ) ->
            Activate.update sharedState subMsg subModel
                |> updateWith ActivatePage ActivateMsg model

        ( NoOp, _ ) ->
            -- Message arrived for wrong page. Ignore that
            ( model, Cmd.none, NoUpdate )

        ( _, _ ) ->
            -- Message arrived for wrong page. Ignore that
            ( model, Cmd.none, NoUpdate )


updateWith :
    (subModel -> Page)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg, SharedStateUpdate )
    -> ( Model, Cmd Msg, SharedStateUpdate )
updateWith toPage toMsg model ( subModel, subCmd, subSharedStateUpdate ) =
    let
        ( newModel, newCmd, newSharedState ) =
            case subSharedStateUpdate of
                RefreshLogin ->
                    -- Intercept the request if a login is needed again
                    ( { model | loginDialogState = True }, Cmd.none, NoUpdate )

                ShowToast toast ->
                    Toasty.addToast Toasty.Defaults.config ToastyMsg toast ( model, Cmd.none )
                        |> Utils.flip Utils.tupleExtend NoUpdate

                UpdateJwtToken maybeJwt rememberMe ->
                    ( model
                    , case maybeJwt of
                        Just token ->
                            Cmd.batch
                                [ getCurrentAccount maybeJwt GetAccountResponse
                                , if rememberMe then
                                    LocalStorage.save jwtAuthenticationTokenKey token

                                  else
                                    Cmd.none
                                ]

                        Nothing ->
                            LocalStorage.clear jwtAuthenticationTokenKey
                    , subSharedStateUpdate
                    )

                _ ->
                    ( model, Cmd.none, subSharedStateUpdate )
    in
    ( { newModel | currentPage = toPage subModel }
    , Cmd.batch
        [ newCmd
        , Cmd.map toMsg subCmd
        ]
    , newSharedState
    )


navigateTo : Route -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
navigateTo route model =
    case route of
        Home ->
            Home.init |> initWith HomePage HomeMsg model NoUpdate

        Login ->
            Login.init |> initWith LoginPage LoginMsg model NoUpdate

        Logout ->
            Logout.init |> initWith LogoutPage LogoutMsg model (UpdateJwtToken Nothing False)

        Register ->
            Register.init |> initWith RegisterPage RegisterMsg model NoUpdate

        PasswordResetRequest ->
            PasswordResetRequest.init |> initWith PasswordResetRequestPage PasswordResetRequestMsg model NoUpdate

        PasswordResetFinish key ->
            PasswordResetFinish.init key |> initWith PasswordResetFinishPage PasswordResetFinishMsg model NoUpdate

        Settings ->
            Settings.init |> initWith SettingsPage SettingsMsg model NoUpdate

        PasswordUpdate ->
            PasswordUpdate.init |> initWith PasswordUpdatePage PasswordUpdateMsg model NoUpdate

        Activate key ->
            Activate.init key |> initWith ActivatePage ActivateMsg model NoUpdate

        NotFound ->
            ( { model | currentPage = NotFoundPage {} }
            , Cmd.none
            , NoUpdate
            )


initWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> SharedStateUpdate -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg, SharedStateUpdate )
initWith toPage toMsg model sharedStateUpdate ( subModel, subCmd ) =
    ( { model | currentPage = toPage subModel }
    , Cmd.map toMsg subCmd
    , sharedStateUpdate
    )
