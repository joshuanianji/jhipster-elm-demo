module Account.Api.SettingsVM exposing (SettingsVM, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias SettingsVM =
    { username : String
    , firstName : String
    , lastName : String
    , email : String
    , languageKey : String
    }


decoder : Decoder SettingsVM
decoder =
    Decode.succeed SettingsVM
        |> required "login" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "email" Decode.string
        |> required "langKey" Decode.string


encoder : SettingsVM -> Encode.Value
encoder model =
    Encode.object
        [ ( "login", Encode.string model.username )
        , ( "firstName", Encode.string model.firstName )
        , ( "lastName", Encode.string model.lastName )
        , ( "email", Encode.string model.email )
        , ( "langKey", Encode.string model.languageKey )
        ]
