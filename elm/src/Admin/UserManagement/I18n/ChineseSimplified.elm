module Admin.UserManagement.I18n.ChineseSimplified exposing (translate)

import Admin.UserManagement.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        UserListTitle ->
            "用户"

        Id ->
            "Id"

        Login ->
            "登录"

        FirstName ->
            "名字"

        LastName ->
            "姓氏"

        Email ->
            "邮箱"

        Language ->
            "语言"

        Role ->
            "角色"

        Activated ->
            "已激活"

        Deactivated ->
            "失效"

        CreatedBy ->
            "创建人"

        CreatedDate ->
            "创建时间"

        LastModifiedBy ->
            "最近修改人"

        LastModifiedDate ->
            "最近修改时间"
