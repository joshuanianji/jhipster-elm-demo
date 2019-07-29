module Shared.I18n.ChineseSimplified exposing (translate)

import Shared.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        MenuHome ->
            "首页"

        MenuTheme ->
            "主题"

        MenuThemeBootstrap ->
            "缺省模式"

        MenuThemeDarkly ->
            "暗黑模式"

        MenuAdmin ->
            "管理"

        MenuAdminUserMgt ->
            "用户管理"

        MenuAdminTracker ->
            "追踪使用者"

        MenuAdminMetrics ->
            "资源监控"

        MenuAdminHealth ->
            "服务状态"

        MenuAdminConfig ->
            "配置"

        MenuAdminLogs ->
            "日志"

        MenuAdminAudits ->
            "审核"

        MenuAdminApi ->
            "API"

        MenuAdminDatabase ->
            "数据库"

        MenuAccount ->
            "账号"

        MenuAccountSettings ->
            "设置"

        MenuAccountPassword ->
            "密码"

        MenuAccountLogin ->
            "登录"

        MenuAccountLogout ->
            "退出"

        MenuAccountRegister ->
            "注册"

        Footer ->
            "这里是页脚"
