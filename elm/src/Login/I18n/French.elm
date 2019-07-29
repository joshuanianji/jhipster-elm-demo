module Login.I18n.French exposing (translate)

import Login.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        LoginTitle ->
            "Authentification"

        UsernameLabel ->
            "Nom d'utilisateur"

        UsernamePlaceholder ->
            "Votre nom d'utilisateur"

        PasswordLabel ->
            "Mot de passe"

        PasswordPlaceholder ->
            "Votre mot de passe"

        RememberMeLabel ->
            "Garder la session ouverte"

        SignInButtonLabel ->
            "Connexion"

        SignInLoadingLabel ->
            "Loading..."

        ForgetPassword ->
            "Avez-vous oublié votre mot de passe ?"

        NoAccountYet ->
            "Vous n'avez pas encore de compte ?"

        RegisterNewAccount ->
            "Créer un compte"

        WrongPasswordOrEmail ->
            "Wrong Password or Username!"

        EmailNotConfirmed ->
            "Your email is not confirmed!"

        FailedLogin ->
            "Erreur d'authentification ! Veuillez vérifier vos identifiants de connexion."

        LoggedIn ->
            "Vous êtes maintenant connecté."

        LoggedInAs username ->
            "Vous êtes connecté en tant que [" ++ username ++ "]."

        LogoutTitle ->
            "Déconnecté avec succès!"

        Success ->
            "Succès"

        Error ->
            "Erreur"

        ServerError ->
            "Erreur du serveur!"
