module Account.I18n.ChineseSimplified exposing (translate)

import Account.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        ActivateTitle ->
            "启用"

        UserAccountCannotActivate ->
            "您的账号无法启用."

        UserAccountActivated ->
            "您的账号已启用."

        MissingActivationKey ->
            "缺少启用码."

        Activating ->
            "启用中..."

        CanLoginNow ->
            "请登录."

        UseRegistrationToSignup ->
            "请重新注册."

        RegisterTitle ->
            "注册"

        RegisterButtonLabel ->
            "注册"

        RegisterButtonLoading ->
            "注册中..."

        RegistrationFailed ->
            "注册失败!"

        RegistrationSuccess ->
            "注册成功! 请检查您的邮箱."

        SettingsTitle username ->
            "[" ++ username ++ "]的用户设置"

        SaveButtonLabel ->
            "保存"

        SaveButtonLoading ->
            "保存中..."

        CannotSaveSettings ->
            "发生错误! 设置无法保存."

        EmailAlreadyInUse ->
            "该邮件地址已被使用! 请使用另外的邮件地址."

        SaveSuccess ->
            "设置保存成功!"

        ResetPasswordTitle ->
            "重置您的密码"

        ResetPasswordInfo ->
            "请输入您注册时使用的邮件地址"

        ResetButtonLabel ->
            "重置密码"

        ResetButtonLoading ->
            "重置中..."

        EmailNotFound ->
            "没有与该邮件地址关联的账号. 请使用其他邮件地址."

        CheckEmail ->
            "已将重置密码的操作说明发送到您的邮箱，请检查邮件."

        MissingResetKey ->
            "无效的重置密码请求!"

        CannotReset ->
            "无法重置密码. 您必须在请求重置密码后24小时内完成重置."

        ResetSuccess ->
            "您的密码已被重置. 请用新密码登录."

        UpdatePasswordTitle username ->
            "[" ++ username ++ "]的密码"

        CannotUpdate ->
            "发生错误! 密码无法被修改."

        UpdateSuccess ->
            "密码修改成功!"

        UsernameLabel ->
            "账号"

        UsernamePlaceholder ->
            "您的账号"

        EmailLabel ->
            "电子邮件"

        EmailPlaceholder ->
            "您的电子邮件"

        CurrentPasswordLabel ->
            "当前密码"

        CurrentPasswordPlaceholder ->
            "您的当前密码"

        NewPasswordLabel ->
            "新密码"

        NewPasswordPlaceholder ->
            "您的新密码"

        ConfirmPasswordLabel ->
            "新密码确认"

        ConfirmPasswordPlaceholder ->
            "确认您的新密码"

        FirstnameLabel ->
            "名字"

        FirstnamePlaceholder ->
            "您的名字"

        LastnameLabel ->
            "姓氏"

        LastnamePlaceholder ->
            "您的姓氏"

        LanguageLabel ->
            "语言"

        PasswordNotMatch ->
            "您输入的密码和确认密码不匹配!"

        Success ->
            "成功"

        Error ->
            "失败"

        ServerError ->
            "服务器错误.!"
