module Login.Common exposing (Context, UiElement, toContext, tt)

import Login.I18n.Phrases as LoginPhrases
import Login.I18n.Translator exposing (translator)
import Shared.Api.User exposing (User)
import Shared.I18n exposing (Language(..))
import Shared.SharedState exposing (SharedState)
import UiFramework exposing (UiContextual, WithContext, uiText)


type alias Context =
    { language : Language
    , translate : LoginPhrases.Phrase -> String
    , user : Maybe User
    }


type alias UiElement msg =
    WithContext Context msg


toContext : SharedState -> UiContextual Context
toContext sharedState =
    { language = sharedState.language
    , translate = translator sharedState.language
    , user = sharedState.user
    , device = sharedState.device
    , themeConfig = sharedState.themeConfig
    , parentRole = Nothing
    }


tt : LoginPhrases.Phrase -> UiElement msg
tt phrase =
    uiText
        (\context -> context.translate phrase)
