module Home.I18n.French exposing (translate)

import Home.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Title ->
            "Bienvenue, Java/Elm Hipster !"

        Subtitle ->
            "Ceci est votre page d'accueil"

        LoggedInAs username ->
            "Vous êtes connecté en tant que [" ++ username ++ "]."

        SignInPrefix ->
            "Si vous voulez vous "

        SignInLink ->
            "connecter"

        SignInSuffix ->
            ", vous pouvez utiliser les comptes par défaut :"

        AdminAccountInfo ->
            "- Administrateur (nom d'utilisateur=\"admin\" et mot de passe =\"admin\")"

        UserAccountInfo ->
            "- Utilisateur (nom d'utilisateur=\"user\" et mot de passe =\"user\")."

        Question ->
            "Si vous avez des questions à propos de JHipster:"

        Like ->
            "Si vous aimez JHipster, donnez nous une étoile sur"
