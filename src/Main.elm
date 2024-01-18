module Main exposing (main)

import Browser exposing (Document)
import Bytes exposing (Bytes)
import Bytes.Decode as Decode exposing (Step(..), decode)
import Bytes.Encode as Encode exposing (encode)
import Html exposing (section, text, textarea)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { encodeInput : String
    , encodeOutput : String
    , decodeInput : String
    , decodeOutput : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { encodeInput = ""
      , encodeOutput = ""
      , decodeInput = ""
      , decodeOutput = ""
      }
    , Cmd.none
    )


type Msg
    = NewEncodeInput String
    | NewDecodeInput String


stringToBytes : String -> Bytes
stringToBytes s =
    encode (Encode.string s)


bytesToList : Bytes -> List Int
bytesToList bytes =
    let
        listStep ( n, xs ) =
            if n <= 0 then
                Decode.succeed (Done xs)

            else
                Decode.unsignedInt8 |> Decode.map (\x -> Loop ( n - 1, x :: xs ))

        bytesDecoder =
            Decode.loop ( Bytes.width bytes, [] ) listStep
    in
    decode bytesDecoder bytes |> Maybe.withDefault []



-- the minimum is 3


perLine : Int
perLine =
    3


toBase8 : Int -> String
toBase8 num =
    let
        inner x acc =
            if x < 8 then
                x :: acc

            else
                inner (x // 8) (modBy 8 x :: acc)
    in
    let
        out =
            inner num []
                |> List.map String.fromInt
                |> String.join ""
    in
    if String.length out < perLine + 1 then
        String.padLeft perLine '0' out

    else
        out


toBase10 : List Int -> Int
toBase10 octal =
    octal
        |> List.reverse
        |> List.indexedMap (\i x -> x * (8 ^ i))
        |> List.sum


toInt : Char -> Int
toInt c =
    case c of
        '🤍' ->
            7

        '🖤' ->
            6

        '💜' ->
            5

        '💙' ->
            4

        '💚' ->
            3

        '💛' ->
            2

        '🧡' ->
            1

        '🤎' ->
            0

        _ ->
            -1000000


fromInt : Char -> Char
fromInt c =
    case c of
        '7' ->
            '🤍'

        '6' ->
            '🖤'

        '5' ->
            '💜'

        '4' ->
            '💙'

        '3' ->
            '💚'

        '2' ->
            '💛'

        '1' ->
            '🧡'

        '0' ->
            '🤎'

        _ ->
            '❓'


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewEncodeInput newInput ->
            ( { model
                | encodeInput = newInput
                , encodeOutput =
                    newInput
                        |> stringToBytes
                        |> bytesToList
                        |> List.map toBase8
                        |> List.map (String.map fromInt)
                        |> String.join " "
              }
            , Cmd.none
            )

        NewDecodeInput newInput ->
            ( { model
                | decodeInput = newInput
                , decodeOutput =
                    newInput
                        |> String.split " "
                        |> List.map String.toList
                        |> List.map (List.map toInt)
                        |> List.map toBase10
                        |> List.map (\x -> encode (Encode.unsignedInt8 x))
                        |> List.map (\x -> decode (Decode.string 1) x)
                        |> List.map (Maybe.withDefault "-")
                        |> List.reverse
                        |> String.join ""
              }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "Hearts of OpenAi"
    , body =
        [ section []
            [ textarea [ onInput NewEncodeInput ] [ text model.encodeInput ]
            , textarea [ disabled True ] [ text model.encodeOutput ]
            ]
        , section []
            [ textarea [ onInput NewDecodeInput ] [ text model.decodeInput ]
            , textarea [ disabled True ] [ text model.decodeOutput ]
            ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
