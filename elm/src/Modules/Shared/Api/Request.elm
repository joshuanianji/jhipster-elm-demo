module Modules.Shared.Api.Request exposing (getCurrentAccount)

import Modules.Shared.Api.Endpoint as Endpoint exposing (unwrap)
import Modules.Shared.Api.Helper exposing (get)
import Modules.Shared.Api.User as User exposing (User)
import RemoteData exposing (RemoteData(..), WebData)


getCurrentAccount : Maybe String -> (WebData User -> msg) -> Cmd msg
getCurrentAccount token toMsg =
    get
        token
        (unwrap Endpoint.account)
        toMsg
        User.decoder
