module Shared.Api.Helper exposing
    ( delete
    , deleteExpectNothing
    , get
    , getExpectNothing
    , patch
    , patchExpectNothing
    , post
    , postExpectNothing
    , postFile
    , put
    , putExpectNothing
    )

import File exposing (File)
import Http
import Json.Decode exposing (Decoder)
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
