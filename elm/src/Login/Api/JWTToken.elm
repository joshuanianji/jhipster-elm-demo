module Login.Api.JWTToken exposing (JWT, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias JWT =
    { token : String
    }


decoder : Decoder JWT
decoder =
    Decode.succeed JWT
        |> required "id_token" Decode.string


encoder : JWT -> Encode.Value
encoder model =
    Encode.object
        [ ( "id_token", Encode.string model.token )
        ]
