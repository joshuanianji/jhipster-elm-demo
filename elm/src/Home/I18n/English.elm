module Home.I18n.English exposing (translate)

import Home.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Title ->
            "Welcome, Java/Elm Hipster!"

        Subtitle ->
            "This is your homepage"

        LoggedInAs username ->
            "You are logged in as user [" ++ username ++ "]."

        SignInPrefix ->
            "If you want to "

        SignInLink ->
            "sign in"

        SignInSuffix ->
            ", you can try the default accounts:"

        AdminAccountInfo ->
            "- Administrator (login=\"admin\" and password=\"admin\")"

        UserAccountInfo ->
            "- User (login=\"user\" and password=\"user\")."

        Question ->
            "If you have any question on JHipster:"

        Like ->
            "If you like JHipster, don't forget to give us a star on"
