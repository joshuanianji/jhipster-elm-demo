module Modules.Login.Common exposing (..)

import Api.Data.User as User exposing (User)
import I18n exposing (Language(..))
import Modules.Login.I18n.Phrases as LoginPhrases
import Modules.Login.I18n.Translator exposing (translator)
import SharedState exposing (SharedState)
import UiFramework exposing (WithContext, UiContextual, toElement, fromElement, uiText, uiRow, uiColumn, uiParagraph, flatMap)
import UiFramework.Colors as Colors


type alias Context =
    { language : Language
    , translate : LoginPhrases.Phrase -> String
    , user : Maybe User
    }


type alias UiElement msg =
    WithContext Context msg


toContext : SharedState -> (UiContextual Context)
toContext sharedState =
        { language = sharedState.language
        , translate = translator sharedState.language
        , user = sharedState.user
        , device = sharedState.device
        , themeColor = Colors.defaultThemeColor
        , parentRole = Nothing
        }


tt : LoginPhrases.Phrase -> UiElement msg
tt phrase =
    uiText
        (\context -> context.translate phrase)

