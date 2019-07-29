module Account.Api.KeyAndPasswordVM exposing (KeyAndPasswordVM, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias KeyAndPasswordVM =
    { key : Maybe String
    , newPassword : Maybe String
    }


decoder : Decoder KeyAndPasswordVM
decoder =
    Decode.succeed KeyAndPasswordVM
        |> optional "key" (Decode.nullable Decode.string) Nothing
        |> optional "newPassword" (Decode.nullable Decode.string) Nothing


encoder : KeyAndPasswordVM -> Encode.Value
encoder model =
    Encode.object
        [ ( "key", maybe Encode.string model.key )
        , ( "newPassword", maybe Encode.string model.newPassword )
        ]
