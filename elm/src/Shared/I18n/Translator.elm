module Shared.I18n.Translator exposing (translator)

import Shared.I18n exposing (Language(..), Translator)
import Shared.I18n.ChineseSimplified as ChineseSimplified
import Shared.I18n.English as English
import Shared.I18n.French as French
import Shared.I18n.Phrases exposing (Phrase)


translator : Language -> Translator Phrase
translator lang =
    case lang of
        ChineseSimplified ->
            ChineseSimplified.translate

        English ->
            English.translate

        French ->
            French.translate
