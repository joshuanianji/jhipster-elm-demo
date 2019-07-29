module Account.Api.RegisterVM exposing (RegisterVM, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias RegisterVM =
    { username : Maybe String
    , email : Maybe String
    , password : Maybe String
    , languageKey : Maybe String
    }


decoder : Decoder RegisterVM
decoder =
    Decode.succeed RegisterVM
        |> optional "login" (Decode.nullable Decode.string) Nothing
        |> optional "email" (Decode.nullable Decode.string) Nothing
        |> optional "password" (Decode.nullable Decode.string) Nothing
        |> optional "langKey" (Decode.nullable Decode.string) Nothing


encoder : RegisterVM -> Encode.Value
encoder model =
    Encode.object
        [ ( "login", maybe Encode.string model.username )
        , ( "email", maybe Encode.string model.email )
        , ( "password", maybe Encode.string model.password )
        , ( "langKey", maybe Encode.string model.languageKey )
        ]
