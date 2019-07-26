module Modules.Account.Api.PasswordUpdateVM exposing (PasswordUpdateVM, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias PasswordUpdateVM =
    { currentPassword : String
    , newPassword : String
    }


decoder : Decoder PasswordUpdateVM
decoder =
    Decode.succeed PasswordUpdateVM
        |> required "currentPassword" Decode.string
        |> required "newPassword" Decode.string


encoder : PasswordUpdateVM -> Encode.Value
encoder model =
    Encode.object
        [ ( "currentPassword", Encode.string model.currentPassword )
        , ( "newPassword", Encode.string model.newPassword )
        ]
