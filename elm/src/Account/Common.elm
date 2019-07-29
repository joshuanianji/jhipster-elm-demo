module Account.Common exposing (Context, UiElement, sharedForms, toContext, tt)

import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Form
import Shared.I18n exposing (Language, languageCode, languageName, supportLanguages)
import Shared.SharedState exposing (SharedState)
import UiFramework exposing (UiContextual, WithContext, uiText)


type alias Context =
    { language : Language
    , translate : AccountPhrases.Phrase -> String
    }


type alias UiElement msg =
    WithContext Context msg


toContext : SharedState -> UiContextual Context
toContext sharedState =
    { language = sharedState.language
    , translate = translator sharedState.language
    , device = sharedState.device
    , themeConfig = sharedState.themeConfig
    , parentRole = Nothing
    }


tt : AccountPhrases.Phrase -> UiElement msg
tt phrase =
    uiText
        (\context -> context.translate phrase)


sharedForms :
    UiContextual Context
    ->
        { usernameField : Form.Form { r | username : String } String
        , firstNameField : Form.Form { r | firstName : String } String
        , lastNameField : Form.Form { r | lastName : String } String
        , emailField : Form.Form { r | email : String } String
        , currentPasswordField : Form.Form { r | currentPassword : String } String
        , passwordField : Form.Form { r | password : String } String
        , repeatPasswordField : Form.Form { r | password : String, repeatPassword : String } ()
        , languageField : Form.Form { r | languageKey : String } String
        }
sharedForms context =
    let
        translate =
            context.translate
    in
    { usernameField =
        Form.textField
            { parser = Ok
            , value = .username
            , update = \value values -> { values | username = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.UsernameLabel
                , placeholder = translate AccountPhrases.UsernamePlaceholder
                }
            }
    , firstNameField =
        Form.textField
            { parser = Ok
            , value = .firstName
            , update = \value values -> { values | firstName = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.FirstnameLabel
                , placeholder = translate AccountPhrases.FirstnamePlaceholder
                }
            }
    , lastNameField =
        Form.textField
            { parser = Ok
            , value = .lastName
            , update = \value values -> { values | lastName = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.LastnameLabel
                , placeholder = translate AccountPhrases.LastnamePlaceholder
                }
            }
    , emailField =
        Form.textField
            { parser = Ok
            , value = .email
            , update = \value values -> { values | email = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.EmailLabel
                , placeholder = translate AccountPhrases.EmailPlaceholder
                }
            }
    , currentPasswordField =
        Form.passwordField
            { parser = Ok
            , value = .currentPassword
            , update = \value values -> { values | currentPassword = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.CurrentPasswordLabel
                , placeholder = translate AccountPhrases.CurrentPasswordPlaceholder
                }
            }
    , passwordField =
        Form.passwordField
            { parser = Ok
            , value = .password
            , update = \value values -> { values | password = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.NewPasswordLabel
                , placeholder = translate AccountPhrases.NewPasswordPlaceholder
                }
            }
    , repeatPasswordField =
        Form.meta
            (\values ->
                Form.passwordField
                    { parser =
                        \value ->
                            if value == values.password then
                                Ok ()

                            else
                                Err <| translate AccountPhrases.PasswordNotMatch
                    , value = .repeatPassword
                    , update =
                        \newValue values_ ->
                            { values_ | repeatPassword = newValue }
                    , error = always Nothing
                    , attributes =
                        { label = translate AccountPhrases.ConfirmPasswordLabel
                        , placeholder = translate AccountPhrases.ConfirmPasswordPlaceholder
                        }
                    }
            )
    , languageField =
        Form.selectField
            { parser = Ok
            , value = .languageKey
            , update = \value values -> { values | languageKey = value }
            , error = always Nothing
            , attributes =
                { label = translate AccountPhrases.LanguageLabel
                , placeholder = ""
                , options =
                    List.map
                        (\lang -> ( languageCode lang, languageName lang ))
                        supportLanguages
                }
            }
    }
