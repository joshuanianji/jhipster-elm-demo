module Login.I18n.English exposing (translate)

import Login.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        LoginTitle ->
            "Sign in"

        UsernameLabel ->
            "Username"

        UsernamePlaceholder ->
            "Your username"

        PasswordLabel ->
            "Password"

        PasswordPlaceholder ->
            "Your password"

        RememberMeLabel ->
            "Remember me"

        SignInButtonLabel ->
            "Sign in"

        SignInLoadingLabel ->
            "Loading..."

        ForgetPassword ->
            "Did you forget your password?"

        NoAccountYet ->
            "You don't have an account yet?"

        RegisterNewAccount ->
            "Register a new account"

        WrongPasswordOrEmail ->
            "Wrong Password or Username!"

        EmailNotConfirmed ->
            "Your email is not confirmed!"

        FailedLogin ->
            "Failed to sign in! Please check your credentials and try again."

        LoggedIn ->
            "You are now logged in."

        LoggedInAs username ->
            "You are logged in as user [" ++ username ++ "]."

        LogoutTitle ->
            "Logged out successfully!"

        Success ->
            "Success"

        Error ->
            "Error"

        ServerError ->
            "Server error!"
