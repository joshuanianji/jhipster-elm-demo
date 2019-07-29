module Shared.SharedState exposing (SharedState, SharedStateUpdate(..), getUsername, update)

import Browser.Navigation
import Element exposing (Device)
import Shared.Api.User exposing (User)
import Shared.I18n exposing (Language(..), languageFromCode)
import Time exposing (Posix, Zone)
import Toasty.Defaults
import UiFramework.Configuration exposing (ThemeConfig)


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , currentTime : Maybe Posix
    , timezone : Maybe Zone
    , language : Language
    , themeConfig : ThemeConfig
    , device : Device
    , jwtToken : Maybe String
    , user : Maybe User
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateTimezone Zone
    | UpdateLanguage Language
    | UpdateTheme ThemeConfig
    | UpdateDevice Device
    | UpdateUser User
    | RefreshLogin
    | ShowToast Toasty.Defaults.Toast
    | UpdateJwtToken (Maybe String) Bool


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = Just time }

        UpdateTimezone zone ->
            { sharedState | timezone = Just zone }

        UpdateLanguage language ->
            { sharedState | language = language }

        UpdateTheme themeConfig ->
            { sharedState | themeConfig = themeConfig }

        UpdateDevice device ->
            { sharedState | device = device }

        UpdateUser user ->
            -- Received for a positive login
            { sharedState
                | user = Just user
                , language = languageFromCode user.languageKey
            }

        UpdateJwtToken maybeJwt _ ->
            case maybeJwt of
                Nothing ->
                    { sharedState | jwtToken = Nothing, user = Nothing }

                Just token ->
                    { sharedState | jwtToken = Just token }

        NoUpdate ->
            sharedState

        RefreshLogin ->
            sharedState

        ShowToast _ ->
            sharedState


getUsername : SharedState -> String
getUsername sharedState =
    case sharedState.user of
        Nothing ->
            ""

        Just user ->
            user.username
