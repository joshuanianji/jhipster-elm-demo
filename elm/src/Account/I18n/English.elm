module Account.I18n.English exposing (translate)

import Account.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        ActivateTitle ->
            "Activation"

        UserAccountCannotActivate ->
            "Your user account could not be activated."

        UserAccountActivated ->
            "Your user account has been activated."

        MissingActivationKey ->
            "The activation key is missing."

        Activating ->
            "Activating"

        CanLoginNow ->
            "You can login now."

        UseRegistrationToSignup ->
            " Please use the registration form to sign up."

        RegisterTitle ->
            "Registration"

        RegisterButtonLabel ->
            "Register"

        RegisterButtonLoading ->
            "Registering..."

        RegistrationFailed ->
            "Registration failed!"

        RegistrationSuccess ->
            "Registration saved! Please check your email for confirmation."

        SettingsTitle username ->
            "User settings for [" ++ username ++ "]"

        SaveButtonLabel ->
            "Save"

        SaveButtonLoading ->
            "Submitting..."

        CannotSaveSettings ->
            "An error has occurred! Settings could not be saved."

        EmailAlreadyInUse ->
            "Email is already in use! Please choose another one."

        SaveSuccess ->
            "Settings saved!"

        ResetPasswordTitle ->
            "Reset your password"

        ResetPasswordInfo ->
            "Enter the email address you used to register"

        ResetButtonLabel ->
            "Reset password"

        ResetButtonLoading ->
            "Submitting..."

        EmailNotFound ->
            "Email address isn't registered! Please check and try again"

        CheckEmail ->
            "Check your emails for details on how to reset your password."

        MissingResetKey ->
            "The reset key is missing!"

        CannotReset ->
            "Your password couldn't be reset. Remember a password request is only valid for 24 hours."

        ResetSuccess ->
            "Your password has been reset. Please login with new password."

        UpdatePasswordTitle username ->
            "Password for [" ++ username ++ "]"

        CannotUpdate ->
            "An error has occurred! The password could not be changed."

        UpdateSuccess ->
            "Password changed!"

        UsernameLabel ->
            "Username"

        UsernamePlaceholder ->
            "Your username"

        EmailLabel ->
            "Email"

        EmailPlaceholder ->
            "Your email"

        CurrentPasswordLabel ->
            "Current password"

        CurrentPasswordPlaceholder ->
            "Your current password"

        NewPasswordLabel ->
            "New password"

        NewPasswordPlaceholder ->
            "Your new password"

        ConfirmPasswordLabel ->
            "New password confirmation"

        ConfirmPasswordPlaceholder ->
            "Confirm the new password"

        FirstnameLabel ->
            "First Name"

        FirstnamePlaceholder ->
            "Your first Name"

        LastnameLabel ->
            "Last Name"

        LastnamePlaceholder ->
            "Your last Name"

        LanguageLabel ->
            "Language"

        PasswordNotMatch ->
            "The password and its confirmation do not match!"

        Success ->
            "Success"

        Error ->
            "Error"

        ServerError ->
            "Something went wrong!"
