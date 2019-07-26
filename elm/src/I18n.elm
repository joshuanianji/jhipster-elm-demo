module I18n exposing (Language(..), Translator, languageCode, languageFromCode, languageName, supportLanguages)


type Language
    = English
    | French
    | ChineseSimplified


type alias Translator phase =
    phase -> String


supportLanguages : List Language
supportLanguages =
    [ English
    , French
    , ChineseSimplified
    ]


languageFromCode : String -> Language
languageFromCode code =
    case code of
        "en" ->
            English

        "fr" ->
            French

        "zh-cn" ->
            ChineseSimplified

        _ ->
            English


languageName : Language -> String
languageName lang =
    case lang of
        ChineseSimplified ->
            "中文(简体)"

        English ->
            "English"

        French ->
            "Français"


languageCode : Language -> String
languageCode lang =
    case lang of
        ChineseSimplified ->
            "zh-cn"

        English ->
            "English"

        French ->
            "Français"
