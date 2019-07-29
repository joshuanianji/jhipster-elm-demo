module Router exposing (Model, Msg(..), init, subscriptions, update, view)

import Account.Activate as Activate
import Account.PasswordResetFinish as PasswordResetFinish
import Account.PasswordResetRequest as PasswordResetRequest
import Account.PasswordUpdate as PasswordUpdate
import Account.Register as Register
import Account.Settings as Settings
import Browser
import Browser.Events as Events
import Browser.Navigation
import Element
    exposing
        ( DeviceClass(..)
        , Element
        , Orientation(..)
        , alignBottom
        , centerY
        , column
        , el
        , fill
        , height
        , html
        , image
        , inFront
        , padding
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rgb255
        , row
        , text
        , width
        )
import Element.Background as Background
import Element.Font as Font
import Error.NotFound as NotFound
import FontAwesome.Brands
import FontAwesome.Solid
import FontAwesome.Styles
import Home.Home as Home
import Html
import Json.Decode as Json
import LocalStorage exposing (Event(..))
import Login.Login as Login
import Login.Logout as Logout
import RemoteData exposing (RemoteData(..), WebData)
import Routes exposing (Route(..), fromUrl, routeToUrlString)
import Shared.Api.Request exposing (getCurrentAccount)
import Shared.Api.User exposing (User)
import Shared.Constants exposing (jwtAuthenticationTokenKey)
import Shared.I18n as I18n
import Shared.I18n.Phrases as GlobalPhrases
import Shared.I18n.Translator exposing (translator)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Task
import Themes.Darkly exposing (darklyThemeConfig)
import Toasty
import Toasty.Defaults
import UiFramework exposing (toElement)
import UiFramework.Colors as Colors
import UiFramework.Configuration exposing (ThemeConfig, defaultThemeConfig)
import UiFramework.Navbar as Navbar
import UiFramework.Toasty
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
                    navigateTo sharedState route model
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


navigateTo : SharedState -> Route -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
navigateTo sharedState route model =
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
            case sharedState.user of
                Nothing ->
                    ( { model | currentPage = NotFoundPage {} }
                    , Cmd.none
                    , NoUpdate
                    )

                Just user ->
                    Settings.init user |> initWith SettingsPage SettingsMsg model NoUpdate

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


view : (Msg -> msg) -> SharedState -> Model -> Browser.Document msg
view msgMapper sharedState model =
    let
        ( title, body ) =
            case model.currentPage of
                NotFoundPage pageModel ->
                    NotFound.view sharedState pageModel |> transform NotFoundMsg

                HomePage pageModel ->
                    Home.view sharedState pageModel |> transform HomeMsg

                LoginPage pageModel ->
                    Login.view sharedState pageModel |> transform LoginMsg

                LogoutPage pageModel ->
                    Logout.view sharedState pageModel |> transform LogoutMsg

                RegisterPage pageModel ->
                    Register.view sharedState pageModel |> transform RegisterMsg

                PasswordResetRequestPage pageModel ->
                    PasswordResetRequest.view sharedState pageModel |> transform PasswordResetRequestMsg

                PasswordResetFinishPage pageModel ->
                    PasswordResetFinish.view sharedState pageModel |> transform PasswordResetFinishMsg

                SettingsPage pageModel ->
                    Settings.view sharedState pageModel |> transform SettingsMsg

                PasswordUpdatePage pageModel ->
                    PasswordUpdate.view sharedState pageModel |> transform PasswordUpdateMsg

                ActivatePage pageModel ->
                    Activate.view sharedState pageModel |> transform ActivateMsg

        navbarState =
            { toggleMenuState = model.toggleMenuState
            , dropdownState = model.dropdownMenuState
            }

        pageLayout content =
            Element.layout []
                (column
                    [ width fill
                    , height fill
                    , inFront <| UiFramework.Toasty.view ToastyMsg model.toasties
                    ]
                    [ FontAwesome.Styles.css |> html
                    , header sharedState navbarState
                    , el
                        [ height fill
                        , width fill
                        , Background.color sharedState.themeConfig.bodyBackground
                        , Font.color <| sharedState.themeConfig.fontColor sharedState.themeConfig.bodyBackground
                        ]
                        content
                    , footer sharedState
                    ]
                )

        transform toMsg =
            Tuple.mapSecond (Element.map toMsg)
                >> Tuple.mapSecond pageLayout
    in
    { title = "jHipster Elm Demo - " ++ title
    , body = List.singleton (Html.map msgMapper body)
    }


header : SharedState -> Navbar.NavbarState DropdownMenuState -> Element Msg
header sharedState navbarState =
    let
        context =
            { device = sharedState.device
            , themeConfig = sharedState.themeConfig
            , parentRole = Nothing
            , state = navbarState
            }

        translate =
            translator sharedState.language

        brand =
            row []
                [ image [ height (px 45) ] { src = "/logo-jhipster.png", description = "Logo" }
                , row [ centerY ]
                    [ el
                        [ padding 0
                        , alignBottom
                        , Font.size 24
                        , Font.bold
                        ]
                        (text "JelmHipster")
                    , el
                        [ paddingEach { top = 0, right = 0, bottom = 0, left = 10 }
                        , alignBottom
                        , Font.size 10
                        ]
                        (text "0.0.1-SNAPSHOT")
                    ]
                ]

        homeMenuItem =
            Navbar.linkItem (NavigateTo Home)
                |> Navbar.withMenuIcon FontAwesome.Solid.home
                |> Navbar.withMenuTitle (translate GlobalPhrases.MenuHome)

        languageMenuItem =
            Navbar.dropdown ToggleLanguageDropdown LanguageOpen
                |> Navbar.withDropdownMenuItems
                    [ Navbar.dropdownMenuLinkItem (SelectLanguage I18n.English)
                        |> Navbar.withDropdownMenuTitle (I18n.languageName I18n.English)
                    , Navbar.dropdownMenuLinkItem (SelectLanguage I18n.French)
                        |> Navbar.withDropdownMenuTitle (I18n.languageName I18n.French)
                    , Navbar.dropdownMenuLinkItem (SelectLanguage I18n.ChineseSimplified)
                        |> Navbar.withDropdownMenuTitle (I18n.languageName I18n.ChineseSimplified)
                    ]
                |> Navbar.DropdownItem
                |> Navbar.withMenuIcon FontAwesome.Solid.flag
                |> Navbar.withMenuTitle (I18n.languageName sharedState.language)

        themeMenuItem =
            Navbar.dropdown ToggleThemeDropdown ThemeOpen
                |> Navbar.withDropdownMenuItems
                    [ Navbar.dropdownMenuLinkItem (SelectTheme defaultThemeConfig)
                        |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuThemeBootstrap)
                    , Navbar.dropdownMenuLinkItem (SelectTheme darklyThemeConfig)
                        |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuThemeDarkly)
                    ]
                |> Navbar.DropdownItem
                |> Navbar.withMenuIcon FontAwesome.Brands.bootstrap
                |> Navbar.withMenuTitle (translate GlobalPhrases.MenuTheme)

        accountMenuItem =
            Navbar.dropdown ToggleAccountDropdown AccountOpen
                |> Navbar.withDropdownMenuItems
                    (case sharedState.user of
                        Just _ ->
                            [ Navbar.dropdownMenuLinkItem (NavigateTo Settings)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.wrench
                                |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuAccountSettings)
                            , Navbar.dropdownMenuLinkItem (NavigateTo PasswordUpdate)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.key
                                |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuAccountPassword)
                            , Navbar.dropdownMenuLinkItem (NavigateTo Logout)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.signOutAlt
                                |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuAccountLogout)
                            ]

                        Nothing ->
                            [ Navbar.dropdownMenuLinkItem (NavigateTo Login)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.signInAlt
                                |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuAccountLogin)
                            , Navbar.dropdownMenuLinkItem (NavigateTo Register)
                                |> Navbar.withDropdownMenuIcon FontAwesome.Solid.cashRegister
                                |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuAccountRegister)
                            ]
                    )
                |> Navbar.DropdownItem
                |> Navbar.withMenuIcon FontAwesome.Solid.user
                |> Navbar.withMenuTitle (translate GlobalPhrases.MenuAccount)
    in
    Navbar.default ToggleMenu
        |> Navbar.withBrand brand
        |> Navbar.withBackgroundColor (Colors.getColor "#353d47")
        |> Navbar.withMenuItems
            [ homeMenuItem
            , languageMenuItem
            , themeMenuItem
            , accountMenuItem
            ]
        |> Navbar.view
        |> toElement context


footer : SharedState -> Element Msg
footer sharedState =
    let
        translate =
            translator sharedState.language
    in
    paragraph
        [ paddingXY 20 10
        , Background.color (rgb255 248 248 248)
        , Font.size 10
        ]
        [ text <| translate GlobalPhrases.Footer ]
