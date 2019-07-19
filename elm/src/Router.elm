module Router exposing(..)

import Api.Data.User as User
import Api.Request.Account exposing (getCurrentAccount)
import Browser
import Browser.Events as Events
import Browser.Navigation exposing (Key)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import FontAwesome.Solid
import FontAwesome.Styles
import Html exposing (Html)
import Html.Events
import I18n
import Json.Decode as Json
import LocalStorage exposing (Event(..), jwtAuthenticationTokenKey)
import Modules.Account.Activate as Activate
import Modules.Home.Home as Home
import Modules.Login.Login as Login
import Modules.Login.Logout as Logout
import Modules.Error.NotFound as NotFound
import Modules.Account.PasswordResetFinish as PasswordResetFinish
import Modules.Account.PasswordResetRequest as PasswordResetRequest
import Modules.Account.PasswordUpdate as PasswordUpdate
import Modules.Account.Register as Register
import Modules.Account.Settings as Settings
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), fromUrl, routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Task
import Toasty
import Toasty.Defaults
import Url exposing (Url)
import UiFramework.Colors as Colors
import UiFramework.Navbar as Navbar
import UiFramework.Toasty
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
    | GetAccountResponse (WebData User.User)
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | ToggleMenu
    | ToggleAccountDropdown
    | ToggleLanguageDropdown
    | CloseDropdown
    | SelectLanguage I18n.Language
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


init : Url  -> ( Model, Cmd Msg )
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


subscriptions : SharedState -> Model -> Sub Msg 
subscriptions sharedState model =
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
                    navigateTo route sharedState model
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

        ( GetAccountResponse (RemoteData.Failure err), _ ) ->
            -- This should not happen?
            -- TODO show toasty error
            ( model
            , Cmd.none
            , NoUpdate
            )


        ( GetAccountResponse (RemoteData.Success user), _ ) ->
            ( model
            , Cmd.none
            , UpdateUser user
            )

        ( GetAccountResponse _, _ )->
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


updateWith : (subModel -> Page)
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


navigateTo : Route -> SharedState -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
navigateTo route sharedState model =
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


-- VIEW --

view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let
        ( title, body ) =
            case model.currentPage of 
                NotFoundPage pageModel ->
                    NotFound.view sharedState pageModel |> transform sharedState NotFoundMsg model

                HomePage pageModel ->
                    Home.view sharedState pageModel |> transform sharedState HomeMsg model

                LoginPage pageModel ->
                    Login.view sharedState pageModel |> transform sharedState LoginMsg model

                LogoutPage pageModel ->
                    Logout.view sharedState pageModel |> transform sharedState LogoutMsg model

                RegisterPage pageModel ->
                    Register.view sharedState pageModel |> transform sharedState RegisterMsg model

                PasswordResetRequestPage pageModel ->
                    PasswordResetRequest.view sharedState pageModel |> transform sharedState PasswordResetRequestMsg model

                PasswordResetFinishPage pageModel ->
                    PasswordResetFinish.view sharedState pageModel |> transform sharedState PasswordResetFinishMsg model

                SettingsPage pageModel ->
                    Settings.view sharedState pageModel |> transform sharedState SettingsMsg model

                PasswordUpdatePage pageModel ->
                    PasswordUpdate.view sharedState pageModel |> transform sharedState PasswordUpdateMsg model

                ActivatePage pageModel ->
                    Activate.view sharedState pageModel |> transform sharedState ActivateMsg model

    in
    { title = "jHipster Elm Demo - " ++ title
    , body = List.singleton (Html.map msgMapper body)
    }


transform : SharedState -> ( a -> Msg ) -> Model -> ( String, Element a) -> ( String, Html.Html Msg )
transform sharedState toMsg model =
    Tuple.mapSecond ( Element.map toMsg ) >> 
    Tuple.mapSecond ( viewLayout sharedState model ) 


viewLayout : SharedState -> Model -> Element Msg -> Html.Html Msg
viewLayout sharedState model content =
    Element.layout 
        []
        ( column 
            [ width fill
            , height fill
            , Background.color Colors.white
            , inFront <| UiFramework.Toasty.view ToastyMsg model.toasties
            ]
            [ FontAwesome.Styles.css |> html
            , header sharedState model
            , el
                [ height fill
                , width fill
                , paddingXY 20 15
                , Background.color Colors.gray100
                , Border.color Colors.black
                ]
                ( el
                    [ height fill
                    , width fill
                    , Background.color <| Colors.getColor "#fafafa"
                    , Border.color Colors.gray400
                    , Border.solid
                    , Border.rounded 4
                    , Border.width 1
                    ]
                    content
                )
            , footer
            ]
        )

header : SharedState -> Model -> Element Msg
header sharedState model =
    Navbar.default ToggleMenu
        |> Navbar.withBrand brand
        |> Navbar.withBackgroundColor (Colors.getColor "#353d47")
        |> Navbar.withMenuItems
            [ Navbar.linkItem (NavigateTo Home)
                |> Navbar.withMenuIcon FontAwesome.Solid.home
                |> Navbar.withMenuTitle "Home"  -- TODO translate
            , Navbar.dropdown ToggleLanguageDropdown LanguageOpen
                |> Navbar.withDropdownMenuItems
                    [ Navbar.dropdownMenuItem (SelectLanguage I18n.English)
                        |> Navbar.withDropdownMenuTitle (I18n.languageName I18n.English)
                    , Navbar.dropdownMenuItem (SelectLanguage I18n.French)
                        |> Navbar.withDropdownMenuTitle (I18n.languageName I18n.French)
                    , Navbar.dropdownMenuItem (SelectLanguage I18n.ChineseSimplified)
                        |> Navbar.withDropdownMenuTitle (I18n.languageName I18n.ChineseSimplified)
                    ]
                |> Navbar.DropdownItem
                |> Navbar.withMenuIcon FontAwesome.Solid.flag
                |> Navbar.withMenuTitle (I18n.languageName sharedState.language)
            , Navbar.dropdown ToggleAccountDropdown AccountOpen
                |> Navbar.withDropdownMenuItems
                    ( case sharedState.user of
                        Just user ->
                            [ Navbar.dropdownMenuItem (NavigateTo Settings)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.wrench
                                |> Navbar.withDropdownMenuTitle "Settings"
                            , Navbar.dropdownMenuItem (NavigateTo PasswordUpdate)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.key
                                |> Navbar.withDropdownMenuTitle "Password"
                            , Navbar.dropdownMenuItem (NavigateTo Logout)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.signOutAlt
                                |> Navbar.withDropdownMenuTitle "Sign out"
                            ]

                        Nothing ->
                            [ Navbar.dropdownMenuItem (NavigateTo Login)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.signInAlt
                                |> Navbar.withDropdownMenuTitle "Sign in"
                            , Navbar.dropdownMenuItem (NavigateTo Register)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.cashRegister
                                |> Navbar.withDropdownMenuTitle "Register"
                            ]
                    )
                |> Navbar.DropdownItem
                |> Navbar.withMenuIcon FontAwesome.Solid.user
                |> Navbar.withMenuTitle "Account"
            ]
        |> Navbar.view
            { deviceClass = sharedState.device.class
            , toggleMenuState = model.toggleMenuState
            , dropdownState = model.dropdownMenuState
            }


footer : Element Msg
footer = 
    paragraph [ paddingXY 20 10, Background.color (rgb255 248 248 248), Font.size 10 ]
        [ text "This is footer." ]

brand : Element Msg
brand =
    row [] [ logo, row [centerY] [project, version ]]


logo : Element Msg
logo =
    image [ height (px 45) ] { src = "/logo-jhipster.png", description = "Logo" }


project : Element Msg
project =
    el
        [ padding 0
        , alignBottom
        , Font.size 24
        , Font.bold
        ]
        ( text "JelmHipster" )


version : Element Msg
version =
    el
        [ paddingEach { top = 0, right = 0, bottom = 0, left = 10}
        , alignBottom
        , Font.size 10
        ]
        ( text "0.0.1-SNAPSHOT" )



isCurrentRoute : Model -> Routes.Route -> Bool
isCurrentRoute model route =
    case ( model.currentPage, route ) of
        ( HomePage _, Routes.Home ) ->
            True

        ( LoginPage _, Routes.Login ) ->
            True

        ( LogoutPage _, Routes.Logout ) ->
            True

        ( RegisterPage _, Routes.Register ) ->
            True

        _ ->
            False


isUnderAccount : Model -> Bool
isUnderAccount model =
    case model.currentPage of
        LoginPage _ ->
            True

        RegisterPage _ ->
            True

        SettingsPage _ ->
            True

        PasswordUpdatePage _ ->
            True

        LogoutPage _ ->
            True

        _ ->
            False