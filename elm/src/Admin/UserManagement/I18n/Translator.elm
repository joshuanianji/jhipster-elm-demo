module Admin.UserManagement.I18n.Translator exposing (translator)

import Admin.UserManagement.I18n.ChineseSimplified as ChineseSimplified
import Admin.UserManagement.I18n.English as English
import Admin.UserManagement.I18n.French as French
import Admin.UserManagement.I18n.Phrases exposing (Phrase)
import Shared.I18n exposing (Language(..), Translator)


translator : Language -> Translator Phrase
translator lang =
    case lang of
        ChineseSimplified ->
            ChineseSimplified.translate

        English ->
            English.translate

        French ->
            French.translate
