module ScriptExport exposing (ParseState(..), Script, ScriptLine, ScriptPiece(..), cueCannonUrl, parseScript, parseScriptHelper, scriptPiecesFromPlainScript)

import Base64
import Json.Encode



--  ____            _       _     _____                       _
-- / ___|  ___ _ __(_)_ __ | |_  | ____|_  ___ __   ___  _ __| |_
-- \___ \ / __| '__| | '_ \| __| |  _| \ \/ / '_ \ / _ \| '__| __|
--  ___) | (__| |  | | |_) | |_  | |___ >  <| |_) | (_) | |  | |_
-- |____/ \___|_|  |_| .__/ \__| |_____/_/\_\ .__/ \___/|_|   \__|
--                   |_|                    |_|
-- Script Export: Turn a script into a link the user can click on


type alias ScriptLine =
    { speaker : String, identifier : String, line : String }


type alias Script =
    { title : String
    , lines : List ScriptLine
    }


cueCannonUrl : Script -> String
cueCannonUrl script =
    let
        baseUrl =
            "cuecannon.com/direct?script="
    in
    scriptEncoder script
        |> Json.Encode.encode 0
        |> Base64.encode
        |> (++) baseUrl


scriptEncoder : Script -> Json.Encode.Value
scriptEncoder { title, lines } =
    Json.Encode.object
        [ ( "lines", Json.Encode.list (lineEncoder title) lines ) ]


lineEncoder : String -> ScriptLine -> Json.Encode.Value
lineEncoder title { speaker, identifier, line } =
    Json.Encode.object
        [ ( "t", Json.Encode.string line )
        , ( "s", Json.Encode.string speaker )
        , ( "l", Json.Encode.string identifier )
        , ( "p", Json.Encode.string title )
        ]



--  ____            _       _     ____                _
-- / ___|  ___ _ __(_)_ __ | |_  |  _ \ __ _ _ __ ___(_)_ __   __ _
-- \___ \ / __| '__| | '_ \| __| | |_) / _` | '__/ __| | '_ \ / _` |
--  ___) | (__| |  | | |_) | |_  |  __/ (_| | |  \__ \ | | | | (_| |
-- |____/ \___|_|  |_| .__/ \__| |_|   \__,_|_|  |___/_|_| |_|\__, |
--                   |_|                                      |___/
-- Script Parsing: Do we have enough info about this script to export it to the app?


type ScriptPiece
    = UnsurePiece String
    | IgnorePiece String
    | CharacterPiece String
    | LinePiece String
    | StageDirectionPiece String


scriptPiecesFromPlainScript : String -> List ScriptPiece
scriptPiecesFromPlainScript plain =
    if plain == "" then
        []

    else
        plain
            |> String.split "\n"
            |> List.map
                (\x ->
                    if x == "" then
                        IgnorePiece x

                    else
                        UnsurePiece x
                )


type ParseState
    = StartingParse
    | Parsed (List ScriptLine)
    | AddingLine (List ScriptLine) { characterName : String, lineSoFar : String }
    | FailedParse String


parseScriptHelper : ScriptPiece -> ParseState -> ParseState
parseScriptHelper scriptPiece state =
    case ( scriptPiece, state ) of
        ( _, FailedParse f ) ->
            FailedParse f

        ( UnsurePiece u, _ ) ->
            FailedParse ("Encountered UnsurePiece: " ++ u)

        ( IgnorePiece _, _ ) ->
            state

        ( StageDirectionPiece _, _ ) ->
            state

        ( CharacterPiece character, StartingParse ) ->
            AddingLine [] { characterName = character, lineSoFar = "" }

        ( CharacterPiece character, Parsed lines ) ->
            AddingLine lines { characterName = character, lineSoFar = "" }

        ( CharacterPiece character, AddingLine lines { characterName, lineSoFar } ) ->
            if lineSoFar == "" then
                FailedParse
                    ("Encountered two Character Pieces in a row: "
                        ++ character
                        ++ " and "
                        ++ characterName
                    )

            else
                Parsed (lines ++ [ { speaker = characterName, identifier = "", line = lineSoFar } ])

        ( LinePiece l, StartingParse ) ->
            FailedParse ("Encountered Line Piece without preceding Character Piece: " ++ l)

        ( LinePiece l, Parsed _ ) ->
            FailedParse ("Encountered Line Piece without preceding Character Piece: " ++ l)

        ( LinePiece l, AddingLine lines { characterName, lineSoFar } ) ->
            AddingLine lines { characterName = characterName, lineSoFar = lineSoFar ++ l }


parseScript : List ScriptPiece -> Result String Script
parseScript scriptPieces =
    case List.foldl parseScriptHelper StartingParse scriptPieces of
        FailedParse s ->
            Err s

        Parsed l ->
            Ok (Script "Exported Script" l)

        AddingLine _ _ ->
            Err "Parse parseScriptHelper ended unexeptedly on AddingLine"

        StartingParse ->
            Err "No script"
