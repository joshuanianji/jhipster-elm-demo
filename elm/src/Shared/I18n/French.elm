module Shared.I18n.French exposing (translate)

import Shared.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        MenuHome ->
            "Accueil"

        MenuTheme ->
            "Thème"

        MenuThemeBootstrap ->
            "Bootstrap"

        MenuThemeDarkly ->
            "Darkly"

        MenuAdmin ->
            "Administration"

        MenuAdminUserMgt ->
            "Gestion des utilisateurs"

        MenuAdminTracker ->
            "Suivi des utilisateurs"

        MenuAdminMetrics ->
            "Métriques"

        MenuAdminHealth ->
            "Diagnostics"

        MenuAdminConfig ->
            "Configuration"

        MenuAdminLogs ->
            "Logs"

        MenuAdminAudits ->
            "Audits"

        MenuAdminApi ->
            "API"

        MenuAdminDatabase ->
            "Base de données"

        MenuAccount ->
            "Compte"

        MenuAccountSettings ->
            "Profil"

        MenuAccountPassword ->
            "Mot de passe"

        MenuAccountLogin ->
            "S'authentifier"

        MenuAccountLogout ->
            "Déconnexion"

        MenuAccountRegister ->
            "Créer un compte"

        Footer ->
            "Ceci est votre pied de page"
