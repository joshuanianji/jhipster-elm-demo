module Admin.UserManagement.Api.User exposing (UserDTO, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra exposing (datetime)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Json.Encode.Extra exposing (maybe)
import Time


type alias UserDTO =
    { id : Int
    , username : String
    , firstName : Maybe String
    , lastName : Maybe String
    , email : String
    , imageUrl : Maybe String
    , activated : Bool
    , languageKey : String
    , createdBy : String
    , createdDate : Maybe Time.Posix
    , lastModifiedBy : String
    , lastModifiedDate : Maybe Time.Posix
    , authorities : List String
    }


decoder : Decoder UserDTO
decoder =
    Decode.succeed UserDTO
        |> required "id" Decode.int
        |> required "login" Decode.string
        |> optional "firstName" (Decode.nullable Decode.string) Nothing
        |> optional "lastName" (Decode.nullable Decode.string) Nothing
        |> required "email" Decode.string
        |> optional "imageUrl" (Decode.nullable Decode.string) Nothing
        |> required "activated" Decode.bool
        |> optional "langKey" Decode.string "en"
        |> required "createdBy" Decode.string
        |> optional "createdDate" (Decode.nullable datetime) Nothing
        |> required "lastModifiedBy" Decode.string
        |> optional "lastModifiedDate" (Decode.nullable datetime) Nothing
        |> required "authorities" (Decode.list Decode.string)


encoder : UserDTO -> Encode.Value
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
