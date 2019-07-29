module Shared.I18n.English exposing (translate)

import Shared.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        MenuHome ->
            "Home"

        MenuTheme ->
            "Theme"

        MenuThemeBootstrap ->
            "Bootstrap"

        MenuThemeDarkly ->
            "Darkly"

        MenuAdmin ->
            "Admin"

        MenuAdminUserMgt ->
            "User Management"

        MenuAdminTracker ->
            "Tracker"

        MenuAdminMetrics ->
            "Metrics"

        MenuAdminHealth ->
            "Health"

        MenuAdminConfig ->
            "Configuration"

        MenuAdminLogs ->
            "Logs"

        MenuAdminAudits ->
            "Audits"

        MenuAdminApi ->
            "API"

        MenuAdminDatabase ->
            "Database"

        MenuAccount ->
            "Account"

        MenuAccountSettings ->
            "Settings"

        MenuAccountPassword ->
            "Password"

        MenuAccountLogin ->
            "Login"

        MenuAccountLogout ->
            "Logout"

        MenuAccountRegister ->
            "Register"

        Footer ->
            "This is your footer"
