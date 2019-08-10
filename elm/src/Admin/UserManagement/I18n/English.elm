module Admin.UserManagement.I18n.English exposing (translate)

import Admin.UserManagement.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        UserListTitle ->
            "Users"

        Id ->
            "Id"

        FirstName ->
            "First name"

        LastName ->
            "Last name"

        Login ->
            "Login"

        Email ->
            "Email"

        Language ->
            "Language"

        Role ->
            "Role"

        Activated ->
            "Activated"

        Deactivated ->
            "Deactivated"

        CreatedBy ->
            "Created by"

        CreatedDate ->
            "Created date"

        LastModifiedBy ->
            "Modified by"

        LastModifiedDate ->
            "Modified date"
