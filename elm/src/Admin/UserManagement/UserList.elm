module Admin.UserManagement.UserList exposing (Model, Msg(..), init, update, view)

import Admin.UserManagement.Api.Request exposing (loadUsers)
import Admin.UserManagement.Api.User exposing (UserDTO)
import Admin.UserManagement.I18n.Phrases as UMPhrases
import Admin.UserManagement.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import DateFormat
import Element exposing (..)
import RemoteData exposing (RemoteData(..))
import Routes exposing (Route(..), routeToUrlString)
import Shared.Api.User exposing (User)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Time
import Toasty.Defaults
import UiFramework exposing (WithContext, fromElement, toElement, uiColumn, uiText)
import UiFramework.Badge as Badge
import UiFramework.Pagination as Pagination
import UiFramework.Table as Table
import UiFramework.Types exposing (Role(..), Size(..))
import UiFramework.Typography exposing (h1)


type alias Model =
    { loadingState : DataLoadingState
    , totalItems : Int
    , sliceSize : Int -- items per slice
    , currentSliceNumber : Int
    , users : List UserDTO
    }


type DataLoadingState
    = Loading
    | Loaded
    | Failed String


type alias Context =
    { translate : UMPhrases.Phrase -> String
    , admin : Maybe User
    }


type alias UiElement msg =
    WithContext Context msg


type Msg
    = NavigateTo Route
    | LoadUsersResponse (RemoteData.WebData { total : Int, list : List UserDTO })
    | SelectPageSlice Int


init : Maybe String -> ( Model, Cmd Msg )
init token =
    ( { loadingState = Loading
      , totalItems = 0
      , sliceSize = 10 -- default 10 items per slice
      , currentSliceNumber = 0
      , users = []
      }
    , loadUsers token { page = 0, size = 10, sort = ( "id", "asc" ) } LoadUsersResponse
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        LoadUsersResponse (RemoteData.Failure _) ->
            ( { model | loadingState = Failed "Load users error" }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error "Error" "error"
            )

        LoadUsersResponse (RemoteData.Success result) ->
            ( { model | loadingState = Loaded, users = result.list, totalItems = result.total }
            , Cmd.none
            , NoUpdate
            )

        LoadUsersResponse _ ->
            ( model, Cmd.none, NoUpdate )

        SelectPageSlice sliceNumber ->
            ( { model | currentSliceNumber = sliceNumber }
            , loadUsers sharedState.jwtToken { page = sliceNumber, size = 10, sort = ( "id", "asc" ) } LoadUsersResponse
            , NoUpdate
            )


tt : UMPhrases.Phrase -> UiElement Msg
tt phrase =
    uiText
        (\context -> context.translate phrase)


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Users"
    , case model.loadingState of
        Loading ->
            text "Loading"

        Loaded ->
            let
                context =
                    { translate = translator sharedState.language
                    , admin = sharedState.user
                    , device = sharedState.device
                    , themeConfig = sharedState.themeConfig
                    , parentRole = Nothing
                    }
            in
            toElement context (content model)

        Failed e ->
            text e
    )


content : Model -> UiElement Msg
content model =
    uiColumn
        [ width fill
        , height fill
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [ paddingXY 0 30, alignLeft ] <|
            tt UMPhrases.UserListTitle
        , case model.loadingState of
            Loading ->
                fromElement (\_ -> text "Loading")

            Loaded ->
                pageableTable model

            Failed e ->
                fromElement (\_ -> text e)
        ]


pageableTable : Model -> UiElement Msg
pageableTable model =
    uiColumn [ spacing 20, width fill ]
        [ userTable model.users
        , userPageable model
        ]


userPageable : Model -> UiElement Msg
userPageable model =
    let
        sliceCount =
            1 + model.totalItems // model.sliceSize

        ( startNumber, endNumber ) =
            if sliceCount <= 5 then
                ( 0, sliceCount - 1 )

            else
                ( max 0 (model.currentSliceNumber - 2)
                , min (sliceCount - 1) (model.currentSliceNumber + 2)
                )

        itemList =
            (if startNumber > 0 then
                [ Pagination.EllipsisItem ]

             else
                []
            )
                ++ List.map (\index -> Pagination.NumberItem index) (List.range startNumber endNumber)
                ++ (if endNumber < (sliceCount - 1) then
                        [ Pagination.EllipsisItem ]

                    else
                        []
                   )
    in
    Pagination.default SelectPageSlice
        |> Pagination.withItems
            itemList
        |> Pagination.withExtraAttrs [ centerX ]
        |> (Pagination.view <|
                { numberOfSlices = sliceCount
                , currentSliceNumber = model.currentSliceNumber
                }
           )


userTable : List UserDTO -> UiElement Msg
userTable users =
    Table.simpleTable
        |> Table.withBordered
        |> Table.withColumns
            [ createColumn UMPhrases.Id (.id >> String.fromInt)
            , createColumn UMPhrases.FirstName (.firstName >> Maybe.withDefault "")
            , createColumn UMPhrases.LastName (.lastName >> Maybe.withDefault "")
            , createColumn UMPhrases.Email .email
            , createColumn UMPhrases.Language .languageKey
            , createRoleColumn
            , createColumn UMPhrases.CreatedBy .createdBy
            , createColumn UMPhrases.CreatedDate (.createdDate >> formatDate)
            , createColumn UMPhrases.LastModifiedBy .lastModifiedBy
            , createColumn UMPhrases.LastModifiedDate (.lastModifiedDate >> formatDate)
            ]
        |> Table.view users


createColumn : UMPhrases.Phrase -> (UserDTO -> String) -> Table.Column UserDTO Context Msg
createColumn headPhrase f =
    { head = tt headPhrase
    , viewData =
        \user ->
            fromElement
                (\_ -> Element.text (f user))
    }


formatDate : Maybe Time.Posix -> String
formatDate maybeDate =
    case maybeDate of
        Nothing ->
            ""

        Just t ->
            DateFormat.format
                [ DateFormat.yearNumber
                , DateFormat.text "-"
                , DateFormat.monthFixed
                , DateFormat.text "-"
                , DateFormat.dayOfMonthNumber
                , DateFormat.text " "
                , DateFormat.hourMilitaryFixed
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                ]
                Time.utc
                t


createRoleColumn : Table.Column UserDTO Context Msg
createRoleColumn =
    { head = tt UMPhrases.Role
    , viewData =
        \user ->
            uiColumn [ spacing 5 ]
                (List.map (\role -> Badge.simple Info role) user.authorities)
    }
