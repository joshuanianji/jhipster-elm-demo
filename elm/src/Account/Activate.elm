module Account.Activate exposing (Model, Msg(..), init, update, view)

import Account.Api.Request exposing (activateAccount)
import Account.Common exposing (UiElement, toContext, tt)
import Account.I18n.Phrases as AccountPhrases
import Account.I18n.Translator exposing (translator)
import Browser.Navigation exposing (pushUrl)
import Element exposing (Element, alignLeft, fill, height, paddingXY, spacing, width)
import Http
import RemoteData
import Routes exposing (Route(..), routeToUrlString)
import Shared.ResponsiveUtils exposing (wrapContent)
import Shared.SharedState exposing (SharedState, SharedStateUpdate(..))
import Toasty.Defaults
import UiFramework exposing (toElement, uiColumn, uiParagraph, uiText)
import UiFramework.Alert as Alert
import UiFramework.Types exposing (Role(..))
import UiFramework.Typography exposing (h1)


type alias Model =
    { key : Maybe String
    , activateState : ActivateState
    }


type ActivateState
    = NoKey
    | Activating
    | Succeeded
    | Failed String


type Msg
    = NavigateTo Route
    | ActivateResponse (RemoteData.WebData ())


init : Maybe String -> ( Model, Cmd Msg )
init maybeKey =
    case maybeKey of
        Nothing ->
            ( { key = Nothing
              , activateState = NoKey
              }
            , Cmd.none
            )

        Just key ->
            ( { key = Just key
              , activateState = Activating
              }
            , activateAccount key ActivateResponse
            )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    let
        translate =
            translator sharedState.language
    in
    case msg of
        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (routeToUrlString route), NoUpdate )

        ActivateResponse (RemoteData.Failure err) ->
            let
                errorString =
                    case err of
                        Http.BadStatus 400 ->
                            translate AccountPhrases.UserAccountCannotActivate

                        _ ->
                            translate AccountPhrases.ServerError
            in
            ( { model | activateState = Failed errorString }
            , Cmd.none
            , ShowToast <| Toasty.Defaults.Error (translate AccountPhrases.Error) errorString
            )

        ActivateResponse (RemoteData.Success ()) ->
            ( { model | activateState = Succeeded }
            , Cmd.none
            , ShowToast <|
                Toasty.Defaults.Success
                    (translate AccountPhrases.Success)
                    (translate AccountPhrases.UserAccountActivated)
            )

        ActivateResponse _ ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> ( String, Element Msg )
view sharedState model =
    ( "Activation"
    , toElement (toContext sharedState) (content model)
    )


content : Model -> UiElement Msg
content model =
    uiColumn
        [ width fill
        , height fill
        , alignLeft
        , paddingXY 20 10
        , spacing 20
        ]
        [ h1 [ paddingXY 0 30 ] <|
            tt AccountPhrases.ActivateTitle
        , case model.activateState of
            NoKey ->
                Alert.simple Warning <|
                    tt AccountPhrases.MissingActivationKey

            Activating ->
                Alert.simple Primary <|
                    tt AccountPhrases.Activating

            Succeeded ->
                Alert.simple Success <|
                    uiParagraph []
                        [ tt AccountPhrases.UserAccountActivated
                        , Alert.link
                            { onPress = Just <| NavigateTo Login
                            , label = tt AccountPhrases.CanLoginNow
                            }
                        ]

            Failed err ->
                Alert.simple Danger <|
                    uiParagraph []
                        [ uiText (\_ -> err)
                        , Alert.link
                            { onPress = Just <| NavigateTo Register
                            , label = tt AccountPhrases.UseRegistrationToSignup
                            }
                        ]
        ]
        |> wrapContent
