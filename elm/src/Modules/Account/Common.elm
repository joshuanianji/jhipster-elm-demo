module Modules.Account.Common exposing (..)

import I18n exposing (Language(..))
import Modules.Account.I18n.Phrases as AccountPhrases
import Modules.Account.I18n.Translator exposing (translator)
import SharedState exposing (SharedState)
import UiFramework exposing (WithContext, UiContextual, toElement, fromElement, uiText, uiRow, uiColumn, uiParagraph, flatMap)
import UiFramework.Colors as Colors


type alias Context =
    { language : Language
    , translate : AccountPhrases.Phrase -> String
    -- , username : String
    }


type alias UiElement msg =
    WithContext Context msg


toContext : SharedState -> (UiContextual Context)
toContext sharedState =
        { language = sharedState.language
        , translate = translator sharedState.language
        -- , username = SharedState.getUsername sharedState
        , device = sharedState.device
        , themeColor = Colors.defaultThemeColor
        , parentRole = Nothing
        }


tt : AccountPhrases.Phrase -> UiElement msg
tt phrase =
    uiText
        (\context -> context.translate phrase)

