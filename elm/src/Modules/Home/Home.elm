module Modules.Home.Home exposing (Model, Msg(..), accountInfo, init, update, view)

import Api.Data.User as User exposing (User)
import Browser.Navigation exposing (pushUrl)
import Element exposing (..)
import Element.Font as Font
import Html
import Html.Attributes
import I18n
import Modules.Home.I18n.Phrases as HomePhrases
import Modules.Home.I18n.Translator exposing (translator)
import Routes exposing (Route(..), routeToUrlString)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import UiFramework exposing (UiContextual, WithContext, flatMap, fromElement, toElement, uiColumn, uiParagraph, uiRow, uiText)
import UiFramework.Alert as Alert
import UiFramework.Colors as Colors
import UiFramework.Padding
import UiFramework.Types exposing (Role(..), ScreenSize(..))
import UiFramework.Typography exposing (h1, textLead)


type alias Model =
    {}


type alias Context =
    { translate : HomePhrases.Phrase -> String
    , user : Maybe User
    }


type alias UiElement msg =
    WithContext Context msg


type Msg
    = NavigateTo Route


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )


toContext : SharedState -> UiContextual Context
toContext sharedState =
    { translate = translator sharedState.language
    , user = sharedState.user
    , device = sharedState.device
    , themeColor = Colors.defaultThemeColor
    , parentRole = Nothing
    }


tt : HomePhrases.Phrase -> UiElement Msg
tt phrase =
    uiText
        (\context -> context.translate phrase)


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Welcome"
    , toElement (toContext sharedState) content
    )


content : UiElement Msg
content =
    uiRow [ width fill, height fill ]
        [ uiColumn
            [ width <| fillPortion 3
            , height fill
            , paddingXY 15 25
            , spacing 20
            ]
            [ h1 [] <| tt HomePhrases.Title
            , textLead [] <| tt HomePhrases.Subtitle
            , accountInfo
            , uiParagraph
                [ Font.alignLeft ]
                [ tt HomePhrases.Like
                , fromElement
                    (\context ->
                        link []
                            { url = "https://github.com/jhipster/generator-jhipster"
                            , label = text "Github"
                            }
                    )
                ]
            ]
        , uiColumn
            [ width <| fillPortion 1
            , alignTop
            ]
            [ fromElement
                (\context ->
                    el [ width fill, height fill ]
                        (Html.img [ Html.Attributes.src "/images/jhipster_family_member_2.svg" ] []
                            |> Element.html
                        )
                )
            ]
        ]
        |> UiFramework.Padding.responsive


accountInfo =
    flatMap
        (\context ->
            case context.user of
                Just user ->
                    withUser user

                Nothing ->
                    withoutUser
        )


withUser user =
    Alert.simple Success <|
        (tt <| HomePhrases.LoggedInAs user.username)


withoutUser =
    Alert.simple Warning <|
        uiColumn
            [ spacing 20 ]
            [ uiParagraph
                [ Font.alignLeft
                ]
                [ tt HomePhrases.SignInPrefix
                , Alert.link
                    { onPress = Just <| NavigateTo Login
                    , label = tt HomePhrases.SignInLink
                    }
                , tt HomePhrases.SignInSuffix
                ]
            , tt HomePhrases.AdminAccountInfo
            , tt HomePhrases.UserAccountInfo
            ]
