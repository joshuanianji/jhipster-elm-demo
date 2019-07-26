module Modules.Shared.Api.User exposing (User, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)


type alias User =
    { id : Int
    , username : String
    , firstName : Maybe String
    , lastName : Maybe String
    , email : String
    , imageUrl : Maybe String
    , activated : Bool
    , languageKey : String
    , authorities : List String
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "id" Decode.int
        |> required "login" Decode.string
        |> optional "firstName" (Decode.nullable Decode.string) Nothing
        |> optional "lastName" (Decode.nullable Decode.string) Nothing
        |> required "email" Decode.string
        |> optional "imageUrl" (Decode.nullable Decode.string) Nothing
        |> required "activated" Decode.bool
        |> required "langKey" Decode.string
        |> required "authorities" (Decode.list Decode.string)


encoder : User -> Encode.Value
encoder model =
    Encode.object
        [ ( "id", Encode.int model.id )
        , ( "login", Encode.string model.username )
        , ( "firstName", maybe Encode.string model.firstName )
        , ( "lastName", maybe Encode.string model.lastName )
        , ( "email", Encode.string model.email )
        , ( "imageUrl", maybe Encode.string model.imageUrl )
        , ( "activated", Encode.bool model.activated )
        , ( "langKey", Encode.string model.languageKey )
        , ( "authorities", Encode.list Encode.string model.authorities )
        ]
