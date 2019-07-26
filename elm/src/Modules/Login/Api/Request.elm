module Modules.Login.Api.Request exposing (authenticate)

import Http
import Modules.Login.Api.JWTToken as JWTToken exposing (JWT)
import Modules.Login.Api.LoginVM as LoginVM exposing (LoginVM)
import Modules.Shared.Api.Endpoint exposing (authentication, unwrap)
import Modules.Shared.Api.Helper exposing (post)
import RemoteData exposing (WebData)


authenticate : LoginVM -> (WebData JWT -> msg) -> Cmd msg
authenticate loginVM toMsg =
    post
        Nothing
        (unwrap authentication)
        (Http.jsonBody (LoginVM.encoder loginVM))
        toMsg
        JWTToken.decoder
