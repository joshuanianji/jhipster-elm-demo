module Account.Api.Request exposing (activateAccount, registerAccount, requestResetPassword, resetPassword, updatePassword, updateSettings)

import Account.Api.KeyAndPasswordVM as KeyAndPasswordVM exposing (KeyAndPasswordVM)
import Account.Api.PasswordUpdateVM as PasswordUpdateVM exposing (PasswordUpdateVM)
import Account.Api.RegisterVM as RegisterVM exposing (RegisterVM)
import Account.Api.SettingsVM as SettingsVM exposing (SettingsVM)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Shared.Api.Endpoint as Endpoint exposing (unwrap)
import Shared.Api.Helper exposing (getExpectNothing, postExpectNothing)


registerAccount : RegisterVM -> (WebData () -> msg) -> Cmd msg
registerAccount registerVM toMsg =
    postExpectNothing
        Nothing
        (unwrap Endpoint.register)
        (Http.jsonBody <| RegisterVM.encoder registerVM)
        toMsg


activateAccount : String -> (WebData () -> msg) -> Cmd msg
activateAccount key toMsg =
    getExpectNothing
        Nothing
        (unwrap Endpoint.activate ++ "?key=" ++ key)
        toMsg


requestResetPassword : String -> (WebData () -> msg) -> Cmd msg
requestResetPassword email toMsg =
    postExpectNothing
        Nothing
        (unwrap Endpoint.resetPasswordRequest)
        (Http.stringBody "text/plain" email)
        toMsg


resetPassword : KeyAndPasswordVM -> (WebData () -> msg) -> Cmd msg
resetPassword keyAndPasswordVM toMsg =
    postExpectNothing
        Nothing
        (unwrap Endpoint.resetPassword)
        (Http.jsonBody <| KeyAndPasswordVM.encoder keyAndPasswordVM)
        toMsg


updateSettings : Maybe String -> SettingsVM -> (WebData () -> msg) -> Cmd msg
updateSettings token settings toMsg =
    postExpectNothing
        token
        (unwrap Endpoint.account)
        (Http.jsonBody <| SettingsVM.encoder settings)
        toMsg


updatePassword : Maybe String -> PasswordUpdateVM -> (WebData () -> msg) -> Cmd msg
updatePassword token passwordUpdateVM toMsg =
    postExpectNothing
        token
        (unwrap Endpoint.changePassword)
        (Http.jsonBody <| PasswordUpdateVM.encoder passwordUpdateVM)
        toMsg
