module Error.I18n.ChineseSimplified exposing (translate)

import Error.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        NotFoundTitle ->
            "该页面不存在."
