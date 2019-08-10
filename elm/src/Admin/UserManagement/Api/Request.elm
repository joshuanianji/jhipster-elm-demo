module Admin.UserManagement.Api.Request exposing (loadUsers)

import Admin.UserManagement.Api.User as User exposing (UserDTO)
import RemoteData exposing (RemoteData(..), WebData)
import Shared.Api.Endpoint as Endpoint exposing (Endpoint, unwrap)
import Shared.Api.Helper exposing (getPageableData)


loadUsers :
    Maybe String
    ->
        { page : Int
        , size : Int
        , sort : ( String, String )
        }
    -> (WebData { total : Int, list : List UserDTO } -> msg)
    -> Cmd msg
loadUsers token param toMsg =
    getPageableData
        token
        (buildUrl Endpoint.users param)
        toMsg
        User.decoder


buildUrl :
    Endpoint
    ->
        { page : Int
        , size : Int
        , sort : ( String, String )
        }
    -> String
buildUrl endpoint param =
    unwrap endpoint
        ++ "?page="
        ++ String.fromInt param.page
        ++ "&size="
        ++ String.fromInt param.size
        ++ "&sort="
        ++ Tuple.first param.sort
        ++ ","
        ++ Tuple.second param.sort
