module Admin.UserManagement.I18n.French exposing (translate)

import Admin.UserManagement.I18n.Phrases exposing (Phrase(..))


translate : Phrase -> String
translate phrase =
    case phrase of
        UserListTitle ->
            "Utilisateurs"

        Id ->
            "Id"

        FirstName ->
            "Prénom"

        LastName ->
            "Nom"

        Login ->
            "Login"

        Email ->
            "Email"

        Language ->
            "Langue"

        Role ->
            "Droits"

        Activated ->
            "Activé"

        Deactivated ->
            "Désactivé"

        CreatedBy ->
            "Créé par"

        CreatedDate ->
            "Créé le"

        LastModifiedBy ->
            "Modifié par"

        LastModifiedDate ->
            "Modifié le"
