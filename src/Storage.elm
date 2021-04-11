port module Storage exposing (decodeScriptPieces, loadScriptPieces, storeScriptPieces)

import Json.Decode as D
import Json.Encode exposing (..)
import ScriptExport exposing (ScriptPiece(..), ScriptPieceKind(..))



--  ____            _       _     ____  _
-- / ___|  ___ _ __(_)_ __ | |_  |  _ \(_) ___  ___ ___  ___
-- \___ \ / __| '__| | '_ \| __| | |_) | |/ _ \/ __/ _ \/ __|
--  ___) | (__| |  | | |_) | |_  |  __/| |  __/ (_|  __/\__ \
-- |____/ \___|_|  |_| .__/ \__| |_|   |_|\___|\___\___||___/
--                   |_|


port storeScriptPiecesValue : Value -> Cmd msg


storeScriptPieces : List ScriptPiece -> Cmd msg
storeScriptPieces pieces =
    storeScriptPiecesValue (encodeScriptPieces pieces)


encodeScriptPieces : List ScriptPiece -> Value
encodeScriptPieces pieces =
    Json.Encode.list encodeScriptPiece pieces


encodeScriptPiece : ScriptPiece -> Value
encodeScriptPiece (ScriptPiece kind line) =
    case kind of
        TitlePiece ->
            object [ ( "kind", string "title" ), ( "line", string line ) ]

        UnsurePiece ->
            object [ ( "kind", string "unsure" ), ( "line", string line ) ]

        IgnorePiece ->
            object [ ( "kind", string "ignore" ), ( "line", string line ) ]

        CharacterPiece ->
            object [ ( "kind", string "character" ), ( "line", string line ) ]

        LinePiece ->
            object [ ( "kind", string "line" ), ( "line", string line ) ]

        StageDirectionPiece ->
            object [ ( "kind", string "stageDirection" ), ( "line", string line ) ]


port loadScriptPieces : (Value -> msg) -> Sub msg


decodeScriptPieces : D.Decoder (List ScriptPiece)
decodeScriptPieces =
    let
        decodeKind kind =
            case kind of
                "title" ->
                    D.succeed TitlePiece

                "unsure" ->
                    D.succeed UnsurePiece

                "ignore" ->
                    D.succeed IgnorePiece

                "character" ->
                    D.succeed CharacterPiece

                "line" ->
                    D.succeed LinePiece

                "stageDirection" ->
                    D.succeed StageDirectionPiece

                _ ->
                    D.fail ("Couldn't decode ScriptPiece kind '" ++ kind ++ "'")
    in
    D.list <|
        D.map2 ScriptPiece
            (D.field "kind" D.string |> D.andThen decodeKind)
            (D.field "line" D.string)
