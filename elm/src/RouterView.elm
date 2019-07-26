module RouterView exposing (view)

import Browser
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
import FontAwesome.Brands
import FontAwesome.Solid
import FontAwesome.Styles
import Html
import I18n
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
import Modules.Shared.I18n.Phrases as GlobalPhrases
import Modules.Shared.I18n.Translator exposing (translator)
import Router exposing (DropdownMenuState(..), Model, Msg(..), Page(..))
import Routes exposing (Route(..))
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Themes.Darkly exposing (darklyThemeConfig)
import UiFramework exposing (toElement)
import UiFramework.Colors as Colors
import UiFramework.Configuration exposing (defaultThemeConfig)
import UiFramework.Navbar as Navbar
import UiFramework.Toasty


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


transform : SharedState -> (a -> Msg) -> Model -> ( String, Element a ) -> ( String, Html.Html Msg )
transform sharedState toMsg model =
    Tuple.mapSecond (Element.map toMsg)
        >> Tuple.mapSecond (viewLayout sharedState model)


viewLayout : SharedState -> Model -> Element Msg -> Html.Html Msg
viewLayout sharedState model content =
    Element.layout
        []
        (column
            [ width fill
            , height fill
            , Background.color sharedState.themeConfig.bodyBackground
            , inFront <| UiFramework.Toasty.view ToastyMsg model.toasties
            ]
            [ FontAwesome.Styles.css |> html
            , header sharedState model
            , el
                [ height fill
                , width fill
                , Background.color sharedState.themeConfig.bodyBackground
                , Font.color <| sharedState.themeConfig.fontColor sharedState.themeConfig.bodyBackground
                ]
                content
            , footer
            ]
        )


header : SharedState -> Model -> Element Msg
header sharedState model =
    let
        context =
            { device = sharedState.device
            , themeConfig = sharedState.themeConfig
            , parentRole = Nothing
            , state =
                { toggleMenuState = model.toggleMenuState
                , dropdownState = model.dropdownMenuState
                }
            }

        translate =
            translator sharedState.language
    in
    Navbar.default ToggleMenu
        |> Navbar.withBrand brand
        |> Navbar.withBackgroundColor (Colors.getColor "#353d47")
        |> Navbar.withMenuItems
            [ Navbar.linkItem (NavigateTo Home)
                |> Navbar.withMenuIcon FontAwesome.Solid.home
                |> Navbar.withMenuTitle (translate GlobalPhrases.MenuHome)

            -- Language
            , Navbar.dropdown ToggleLanguageDropdown LanguageOpen
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

            -- Theme
            , Navbar.dropdown ToggleThemeDropdown ThemeOpen
                |> Navbar.withDropdownMenuItems
                    [ Navbar.dropdownMenuLinkItem (SelectTheme defaultThemeConfig)
                        |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuThemeBootstrap)
                    , Navbar.dropdownMenuLinkItem (SelectTheme darklyThemeConfig)
                        |> Navbar.withDropdownMenuTitle (translate GlobalPhrases.MenuThemeDarkly)
                    ]
                |> Navbar.DropdownItem
                |> Navbar.withMenuIcon FontAwesome.Brands.bootstrap
                |> Navbar.withMenuTitle (translate GlobalPhrases.MenuTheme)

            -- Account
            , Navbar.dropdown ToggleAccountDropdown AccountOpen
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
            ]
        |> Navbar.view
        |> toElement context


footer : Element Msg
footer =
    paragraph [ paddingXY 20 10, Background.color (rgb255 248 248 248), Font.size 10 ]
        [ text "This is footer." ]


brand : Element Msg
brand =
    row [] [ logo, row [ centerY ] [ project, version ] ]


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
        (text "JelmHipster")


version : Element Msg
version =
    el
        [ paddingEach { top = 0, right = 0, bottom = 0, left = 10 }
        , alignBottom
        , Font.size 10
        ]
        (text "0.0.1-SNAPSHOT")
