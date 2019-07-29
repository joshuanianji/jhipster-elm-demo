module Account.I18n.Phrases exposing (Phrase(..))


type
    Phrase
    -- Activate page
    = ActivateTitle
    | UserAccountCannotActivate
    | UserAccountActivated
    | MissingActivationKey
    | Activating
    | CanLoginNow
    | UseRegistrationToSignup
      -- Registration page
    | RegisterTitle
    | RegisterButtonLabel
    | RegisterButtonLoading
    | RegistrationFailed
    | RegistrationSuccess
      -- Settings page
    | SettingsTitle String
    | SaveButtonLabel
    | SaveButtonLoading
    | CannotSaveSettings
    | EmailAlreadyInUse
    | SaveSuccess
      -- Reset Password page
    | ResetPasswordTitle
    | ResetPasswordInfo
    | ResetButtonLabel
    | ResetButtonLoading
    | EmailNotFound
    | CheckEmail
    | MissingResetKey
    | CannotReset
    | ResetSuccess
      -- Update Password page
    | UpdatePasswordTitle String
    | CannotUpdate
    | UpdateSuccess
      -- form fields
    | UsernameLabel
    | UsernamePlaceholder
    | EmailLabel
    | EmailPlaceholder
    | CurrentPasswordLabel
    | CurrentPasswordPlaceholder
    | NewPasswordLabel
    | NewPasswordPlaceholder
    | ConfirmPasswordLabel
    | ConfirmPasswordPlaceholder
    | FirstnameLabel
    | FirstnamePlaceholder
    | LastnameLabel
    | LastnamePlaceholder
    | LanguageLabel
      -- form validation error messages
    | PasswordNotMatch
      -- General messages
    | Success
    | Error
    | ServerError
