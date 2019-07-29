module Shared.Api.Request exposing (getCurrentAccount)

import RemoteData exposing (RemoteData(..), WebData)
import Shared.Api.Endpoint as Endpoint exposing (unwrap)
import Shared.Api.Helper exposing (get)
import Shared.Api.User as User exposing (User)


getCurrentAccount : Maybe String -> (WebData User -> msg) -> Cmd msg
getCurrentAccount token toMsg =
    get
        token
        (unwrap Endpoint.account)
        toMsg
        User.decoder
