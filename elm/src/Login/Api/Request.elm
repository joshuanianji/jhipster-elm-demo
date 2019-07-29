module Login.Api.Request exposing (authenticate)

import Http
import Login.Api.JWTToken as JWTToken exposing (JWT)
import Login.Api.LoginVM as LoginVM exposing (LoginVM)
import RemoteData exposing (WebData)
import Shared.Api.Endpoint exposing (authentication, unwrap)
import Shared.Api.Helper exposing (post)


authenticate : LoginVM -> (WebData JWT -> msg) -> Cmd msg
authenticate loginVM toMsg =
    post
        Nothing
        (unwrap authentication)
        (Http.jsonBody (LoginVM.encoder loginVM))
        toMsg
        JWTToken.decoder
