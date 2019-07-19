module Modules.Error.Common exposing (..)

import Api.Data.User as User exposing (User)
import I18n exposing (Language(..))
import Modules.Error.I18n.Phrases as ErrorPhrases
import Modules.Error.I18n.Translator exposing (translator)
import SharedState exposing (SharedState)
import UiFramework exposing (WithContext, UiContextual, toElement, fromElement, uiText, uiRow, uiColumn, uiParagraph, flatMap)
import UiFramework.Colors as Colors


type alias Context =
    { language : Language
    , translate : ErrorPhrases.Phrase -> String
    }


type alias UiElement msg =
    WithContext Context msg


toContext : SharedState -> (UiContextual Context)
toContext sharedState =
        { language = sharedState.language
        , translate = translator sharedState.language
        , device = sharedState.device
        , themeColor = Colors.defaultThemeColor
        , parentRole = Nothing
        }


tt : ErrorPhrases.Phrase -> UiElement msg
tt phrase =
    uiText
        (\context -> context.translate phrase)

