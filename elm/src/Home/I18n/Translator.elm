module Home.I18n.Translator exposing (translator)

import Home.I18n.ChineseSimplified as ChineseSimplified
import Home.I18n.English as English
import Home.I18n.French as French
import Home.I18n.Phrases exposing (Phrase)
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
