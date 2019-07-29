module Home.I18n.ChineseSimplified exposing (translate)

import Home.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        Title ->
            "欢迎, Java/Elm Hipster!"

        Subtitle ->
            "这里是首页"

        LoggedInAs username ->
            "您目前是以 [" ++ username ++ "] 账号登录."

        SignInPrefix ->
            "如果您要"

        SignInLink ->
            "登录"

        SignInSuffix ->
            ", 您可以使用默认账号:"

        AdminAccountInfo ->
            "- 管理员 (账号=\"admin\"和密码=\"admin\")"

        UserAccountInfo ->
            "- 普通用户 (账号=\"user\"和密码=\"user\")."

        Question ->
            "如果您有任何有关 JHipster 的问题, 可以查阅下列资源:"

        Like ->
            "如果您喜欢 JHipster, 请记得给我们加星在"
