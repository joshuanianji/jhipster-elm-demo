module Login.Api.LoginVM exposing (LoginVM, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias LoginVM =
    { username : Maybe String
    , password : Maybe String
    , rememberMe : Bool
    }


decoder : Decoder LoginVM
decoder =
    Decode.succeed LoginVM
        |> optional "username" (Decode.nullable Decode.string) Nothing
        |> optional "password" (Decode.nullable Decode.string) Nothing
        |> optional "rememberMe" Decode.bool False


encoder : LoginVM -> Encode.Value
encoder model =
    Encode.object
        [ ( "username", maybe Encode.string model.username )
        , ( "password", maybe Encode.string model.password )
        , ( "rememberMe", Encode.bool model.rememberMe )
        ]
