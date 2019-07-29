module Account.I18n.French exposing (translate)

import Account.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        ActivateTitle ->
            "Activation"

        UserAccountCannotActivate ->
            "Votre compte utilisateur n'a pas pu être activé."

        UserAccountActivated ->
            "Votre compte utilisateur a été activé."

        MissingActivationKey ->
            "La clé d'activation est manquante."

        Activating ->
            "L'activation"

        CanLoginNow ->
            "Merci de vous connecter."

        UseRegistrationToSignup ->
            "Utilisez le formulaire d'enregistrement pour en créer un nouveau."

        RegisterTitle ->
            "Création de compte utilisateur"

        RegisterButtonLabel ->
            "Enregistrement"

        RegisterButtonLoading ->
            "Enregistrement..."

        RegistrationFailed ->
            "Compte non créé !"

        RegistrationSuccess ->
            "Compte enregistré ! Merci de vérifier votre email de confirmation."

        SettingsTitle username ->
            "Profil de l'utilisateur [" ++ username ++ "]"

        SaveButtonLabel ->
            "Sauvegarder"

        SaveButtonLoading ->
            "Soumission..."

        CannotSaveSettings ->
            "Une erreur est survenue ! Votre profil n'a pas été sauvegardé."

        EmailAlreadyInUse ->
            "Cet email est déjà utilisé ! Veuillez en choisir un autre."

        SaveSuccess ->
            "Votre profil a été sauvegardé !"

        ResetPasswordTitle ->
            "Réinitialiser son mot de passe"

        ResetPasswordInfo ->
            "Veuillez renseigner l'adresse email utilisée pour vous enregistrer"

        ResetButtonLabel ->
            "Réinitialiser le mot de passe"

        ResetButtonLoading ->
            "Soumission..."

        EmailNotFound ->
            "L'adresse email n'existe pas ! Merci de la vérifier et de réessayer."

        CheckEmail ->
            "Veuillez vérifier vos nouveaux emails et suivre les instructions pour réinitialiser votre mot de passe."

        MissingResetKey ->
            "La clef de réinitilisation est manquante!"

        CannotReset ->
            "Votre mot de passe n'a pas pu être réinitialisé. La demande de réinitialisation n'est valable que 24 heures."

        ResetSuccess ->
            "Votre mot de passe a été réinitialisé. Veuillez vous connecter avec un nouveau mot de passe."

        UpdatePasswordTitle username ->
            "Changer le mot de passe pour [" ++ username ++ "]"

        CannotUpdate ->
            "Une erreur est survenue ! Le mot de passe n'a pas pu être modifié."

        UpdateSuccess ->
            "Le mot de passe a été modifié !"

        UsernameLabel ->
            "Nom d'utilisateur"

        UsernamePlaceholder ->
            "Votre nom d'utilisateur"

        EmailLabel ->
            "Email"

        EmailPlaceholder ->
            "Votre email"

        CurrentPasswordLabel ->
            "Mot de passe actuel"

        CurrentPasswordPlaceholder ->
            "Mot de passe actuel"

        NewPasswordLabel ->
            "Nouveau mot de passe"

        NewPasswordPlaceholder ->
            "Nouveau mot de passe"

        ConfirmPasswordLabel ->
            "Confirmation du nouveau mot de passe"

        ConfirmPasswordPlaceholder ->
            "Confirmation du nouveau mot de passe"

        FirstnameLabel ->
            "Prénom"

        FirstnamePlaceholder ->
            "Votre prénom"

        LastnameLabel ->
            "Nom"

        LastnamePlaceholder ->
            "Votre nom"

        LanguageLabel ->
            "Langue"

        PasswordNotMatch ->
            "Le nouveau mot de passe et sa confirmation ne sont pas égaux !"

        Success ->
            "Success"

        Error ->
            "Error"

        ServerError ->
            "Something went wrong!"
