module UiFramework.Password exposing (strengthBubbles, validate)

{-| This module deals with two things:

  - Validating passwords for strength

  - producing the "password strength bubbles" in the form of Element msg

    wait no it shouldn't deal with two things it should only deal with one thing

-}

import Element exposing (Color, Element)
import Element.Background as Background
import Element.Border as Border


validate : String -> Result String String
validate password =
    if String.length password <= 4 then
        Err <| "Password must be greater than 4 characters long"

    else
        case passwordStrength password of
            Weakest ->
                Err <| "Password Too Weak!"

            _ ->
                Ok "Yee Haw"



-- the magnum opus of this module


strengthBubbles : String -> Element msg
strengthBubbles password =
    password
        |> passwordStrength
        |> splitAndColor bubbleStrengthTuples
        |> List.map render
        |> Element.row
            [ Element.spacing 5 ]


splitAndColor : List ( Bubble, Strength ) -> Strength -> List Bubble
splitAndColor list strength =
    let
        -- splits list
        helper stronk colored remaining =
            case remaining of
                ( b, s ) :: xy ->
                    if stronk == s then
                        ( colored ++ [ ( b, s ) ], xy )

                    else
                        helper stronk (colored ++ [ ( b, s ) ]) xy

                [] ->
                    -- shouldn't happen
                    ( colored, remaining ) |> Debug.log "bruh"
    in
    -- unzip tuple and discard the Strengths connected to them
    helper strength [] list
        |> (\( coloredTuples, remainingTuples ) ->
                ( List.unzip coloredTuples |> Tuple.first |> colorBubbles (strengthToColor strength)
                , List.unzip remainingTuples |> Tuple.first
                )
           )
        |> (\( colored, remaining ) ->
                colored ++ remaining
           )


colorBubbles : Color -> List Bubble -> List Bubble
colorBubbles color bubbles =
    List.map
        (\b -> changeColor color b)
        bubbles



-- used to create the password bubbles


bubbleStrengthTuples : List ( Bubble, Strength )
bubbleStrengthTuples =
    [ ( default, Weakest )
    , ( default, Weak )
    , ( default, Normal )
    , ( default, Strong )
    , ( default, Strongest )
    ]



-- BUBBLE TYPE --


type Bubble
    = Bubble BubbleOptions



-- only thing they can change is the color lol


type alias BubbleOptions =
    { color : Color
    }


default : Bubble
default =
    Bubble { color = Element.rgb255 221 221 221 }


changeColor : Color -> Bubble -> Bubble
changeColor color (Bubble options) =
    Bubble { options | color = color }



-- render a bubble. I have a lot of "preset" attributes but idk what to do with them.


render : Bubble -> Element msg
render (Bubble options) =
    Element.el
        [ Background.color options.color
        , Border.rounded 15
        , Element.height <| Element.px <| 5
        , Element.width <| Element.px <| 20
        ]
        Element.none



-- STRENGTH TYPE --


type Strength
    = Weakest
    | Weak
    | Normal
    | Strong
    | Strongest


passwordStrength : String -> Strength
passwordStrength password =
    let
        matches =
            List.map
                (\match -> match password)
                [ num, lowerLetters, upperLetters, symbols ]

        -- number of true matches
        passedMatches =
            List.filter identity matches
                |> List.length

        calc =
            \passwordLength ->
                2
                    * passwordLength
                    + (passedMatches * 10)

        -- penalty for a short password and a poor variety of characters
        applyPenalty =
            (\passwordLength ->
                if passwordLength <= 6 then
                    passwordLength
                        |> min 10

                else
                    passwordLength
            )
                >> (if passedMatches == 1 then
                        min 10

                    else if passedMatches == 2 then
                        min 20

                    else if passedMatches == 3 then
                        min 40

                    else
                        identity
                   )

        numStrength =
            String.length password
                |> calc
                |> applyPenalty
    in
    if numStrength <= 10 then
        Weakest

    else if numStrength <= 20 then
        Weak

    else if numStrength <= 30 then
        Normal

    else if numStrength <= 40 then
        Strong

    else
        Strongest


num : String -> Bool
num =
    String.any Char.isDigit


lowerLetters : String -> Bool
lowerLetters =
    String.any Char.isLower


upperLetters : String -> Bool
upperLetters =
    String.any Char.isUpper


symbols : String -> Bool
symbols =
    String.any (Char.isAlphaNum >> not)


strengthToColor : Strength -> Color
strengthToColor strength =
    case strength of
        Weakest ->
            Element.rgb255 255 0 0

        Weak ->
            Element.rgb255 255 153 0

        Normal ->
            Element.rgb255 255 255 0

        Strong ->
            Element.rgb255 153 255 0

        Strongest ->
            Element.rgb255 0 255 0


strengthToString : Strength -> String
strengthToString strength =
    case strength of
        Weakest ->
            "Weakest"

        Weak ->
            "Weak"

        Normal ->
            "Ok"

        Strong ->
            "Strong"

        Strongest ->
            "Strongest"
