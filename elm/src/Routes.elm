module Routes exposing (Route(..), fromUrl, routeToUrlString)

import Url
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, query, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Login
    | Logout
    | Register
    | PasswordResetRequest
    | PasswordResetFinish (Maybe String)
    | Settings
    | PasswordUpdate
    | Activate (Maybe String)
    | NotFound


fromUrl : Url.Url -> Route
fromUrl url =
    parse routeParser url
        |> Maybe.withDefault NotFound


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map Logout (s "logout")
        , map Register (s "register")
        , map PasswordResetRequest (s "reset" </> s "request")
        , map PasswordResetFinish (s "reset" </> s "finish" </> query (Query.string "key"))
        , map Settings (s "account" </> s "settings")
        , map PasswordUpdate (s "account" </> s "password")
        , map Activate (s "activate" </> query (Query.string "key"))
        ]



{-
   This is the util function to translate Route data into url string.
-}


routeToUrlString : Route -> String
routeToUrlString route =
    let
        pieces =
            case route of
                Home ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                PasswordResetRequest ->
                    [ "reset", "request" ]

                PasswordResetFinish _ ->
                    [ "reset", "finish" ]

                Settings ->
                    [ "account", "settings" ]

                PasswordUpdate ->
                    [ "account", "password" ]

                Activate _ ->
                    [ "activate" ]

                NotFound ->
                    []
    in
    "/" ++ String.join "/" pieces
