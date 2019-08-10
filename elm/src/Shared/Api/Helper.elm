module Shared.Api.Helper exposing
    ( delete
    , deleteExpectNothing
    , get
    , getExpectNothing
    , getPageableData
    , patch
    , patchExpectNothing
    , post
    , postExpectNothing
    , postFile
    , put
    , putExpectNothing
    )

import Dict
import File exposing (File)
import Http
import Json.Decode as Decode exposing (Decoder)
import RemoteData exposing (RemoteData(..), WebData)


authHeader : Maybe String -> List Http.Header
authHeader jwtToken =
    case jwtToken of
        Nothing ->
            []

        Just token ->
            [ Http.header "Authorization" ("Bearer " ++ token) ]


get : Maybe String -> String -> (WebData a -> msg) -> Decoder a -> Cmd msg
get jwtToken url msg decoder =
    Http.request
        { method = "GET"
        , headers = authHeader jwtToken
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


getPageableData : Maybe String -> String -> (WebData { total : Int, list : List a } -> msg) -> Decoder a -> Cmd msg
getPageableData jwtToken url msg decoder =
    Http.request
        { method = "GET"
        , headers = authHeader jwtToken
        , url = url
        , body = Http.emptyBody
        , expect =
            Http.expectStringResponse (RemoteData.fromResult >> msg) <|
                \response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Err (Http.BadUrl url_)

                        Http.Timeout_ ->
                            Err Http.Timeout

                        Http.NetworkError_ ->
                            Err Http.NetworkError

                        Http.BadStatus_ metadata body ->
                            Err (Http.BadStatus metadata.statusCode)

                        Http.GoodStatus_ metadata body ->
                            let
                                totalItemCount =
                                    metadata.headers
                                        |> Dict.get "x-total-count"
                                        |> Maybe.andThen
                                            (\countStr ->
                                                countStr
                                                    |> Decode.decodeString Decode.int
                                                    |> Result.withDefault 10
                                                    |> Just
                                            )
                                        |> Maybe.withDefault 10

                                itemList =
                                    Decode.decodeString (Decode.list decoder) body
                            in
                            case itemList of
                                Ok list ->
                                    Ok { total = totalItemCount, list = list }

                                Err error ->
                                    Err (Http.BadBody (Decode.errorToString error))
        , timeout = Nothing
        , tracker = Nothing
        }


getExpectNothing : Maybe String -> String -> (WebData () -> msg) -> Cmd msg
getExpectNothing jwtToken url msg =
    Http.request
        { method = "GET"
        , headers = authHeader jwtToken
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


post : Maybe String -> String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
post jwtToken url body msg decoder =
    Http.request
        { method = "POST"
        , headers = authHeader jwtToken
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postExpectNothing : Maybe String -> String -> Http.Body -> (WebData () -> msg) -> Cmd msg
postExpectNothing jwtToken url body msg =
    Http.request
        { method = "POST"
        , headers = authHeader jwtToken
        , url = url
        , body = body
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Uploads a single file. You can subscribe to the tracker "file\_upload" using

    type Msg
        = GotProgress Http.Progress

    subscriptions : Model -> Sub Msg
    subscriptions model =
        Http.track "file_upload" GotProgress

-}
postFile : String -> File -> (WebData () -> msg) -> Cmd msg
postFile url file msg =
    Http.request
        { method = "POST"
        , headers = []
        , url = url
        , body = Http.multipartBody [ Http.filePart "file_data" file ]
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Just "file_upload"
        }


patch : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
patch url body msg decoder =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


patchExpectNothing : String -> Http.Body -> (WebData () -> msg) -> Cmd msg
patchExpectNothing url body msg =
    Http.request
        { method = "PATCH"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


put : String -> Http.Body -> (WebData a -> msg) -> Decoder a -> Cmd msg
put url body msg decoder =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


putExpectNothing : String -> Http.Body -> (WebData () -> msg) -> Cmd msg
putExpectNothing url body msg =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }


delete : String -> (WebData a -> msg) -> Decoder a -> Cmd msg
delete url msg decoder =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteExpectNothing : String -> (WebData () -> msg) -> Cmd msg
deleteExpectNothing url msg =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectWhatever (RemoteData.fromResult >> msg)
        , timeout = Nothing
        , tracker = Nothing
        }
