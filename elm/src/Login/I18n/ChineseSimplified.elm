module Login.I18n.ChineseSimplified exposing (translate)

import Login.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        LoginTitle ->
            "登录"

        UsernameLabel ->
            "账号"

        UsernamePlaceholder ->
            "您的账号"

        PasswordLabel ->
            "密码"

        PasswordPlaceholder ->
            "您的密码"

        RememberMeLabel ->
            "自动登录"

        SignInButtonLabel ->
            "登录"

        SignInLoadingLabel ->
            "登录中。。。"

        ForgetPassword ->
            "忘记密码?"

        NoAccountYet ->
            "您还没有账号?"

        RegisterNewAccount ->
            "注册一个新账号"

        WrongPasswordOrEmail ->
            "Wrong Password or Username!"

        EmailNotConfirmed ->
            "Your email is not confirmed!"

        FailedLogin ->
            "登录失败! 请检查您的登录信息, 并重试一次."

        LoggedIn ->
            "您已经登录"

        LoggedInAs username ->
            "您目前是以 [" ++ username ++ "] 账号登录."

        LogoutTitle ->
            "成功退出!"

        Success ->
            "成功"

        Error ->
            "失败"

        ServerError ->
            "服务器错误!"
