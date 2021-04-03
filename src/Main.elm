module Main exposing (main)

import Base64
import Color
import Element exposing (Element)
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import Json.Encode
import Material.Icons exposing (offline_bolt)
import Material.Icons.Types exposing (Coloring(..))
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Platform
import Widget.Icon exposing (Icon)



--     _
--    / \   _ __  _ __
--   / _ \ | '_ \| '_ \
--  / ___ \| |_) | |_) |
-- /_/   \_\ .__/| .__/
--         |_|   |_|
-- the intellij-elm plugin doesn't support type aliases for Programs so we need to use this line
-- main : Platform.Program Pages.Platform.Flags (Pages.Platform.Model Model Msg Metadata Rendered) (Pages.Platform.Msg Msg Metadata Rendered)


main : Pages.Platform.Program Model Msg Metadata Rendered Pages.PathKey
main =
    Pages.Platform.init
        { init = \_ -> init
        , view = view scriptParseApp
        , update = update
        , subscriptions = subscriptions
        , documents = [ markdownDocument ]
        , manifest = manifest
        , canonicalSiteUrl = canonicalSiteUrl
        , onPageChange = Nothing
        , internals = Pages.internals
        }
        |> Pages.Platform.withFileGenerator generateFiles
        |> Pages.Platform.toProgram


type alias Rendered =
    Element Msg


type Msg
    = Change String


type alias Model =
    { plainScript : String, scriptPieces : List ScriptPiece }


testString1 =
    "This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius. This is the dawning of the Age of Aquarius. "


init : ( Model, Cmd Msg )
init =
    ( { plainScript = testString1
      , scriptPieces = scriptPiecesFromPlainScript testString1
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change s ->
            ( { plainScript = s
              , scriptPieces = scriptPiecesFromPlainScript s
              }
            , Cmd.none
            )


subscriptions : Metadata -> pages -> Model -> Sub msg
subscriptions _ _ _ =
    Sub.none



--  ____            _       _     _       _             __
-- / ___|  ___ _ __(_)_ __ | |_  (_)_ __ | |_ ___ _ __ / _| __ _  ___ ___
-- \___ \ / __| '__| | '_ \| __| | | '_ \| __/ _ \ '__| |_ / _` |/ __/ _ \
--  ___) | (__| |  | | |_) | |_  | | | | | ||  __/ |  |  _| (_| | (_|  __/
-- |____/ \___|_|  |_| .__/ \__| |_|_| |_|\__\___|_|  |_|  \__,_|\___\___|
--                   |_|


scriptParseApp : Model -> { title : String, body : List (Element Msg) }
scriptParseApp model =
    { title = "Script Parser"
    , body =
        [ Element.column [ Element.width Element.fill ]
            [ Element.row []
                [ Element.el [ Element.width Element.fill ] <| scriptEditor model
                , Element.el [ Element.width Element.fill, Element.alignTop ] <| scriptPiecesView model
                ]
            ]
        ]
    }


scriptEditor : Model -> Element Msg
scriptEditor { plainScript } =
    Element.el [ Element.width Element.fill ] <|
        Element.Input.multiline []
            { onChange = Change
            , text = plainScript
            , placeholder = Nothing
            , label = Element.Input.labelAbove [] <| Element.text ""
            , spellcheck = False
            }


scriptPiecesView : Model -> Element Msg
scriptPiecesView { scriptPieces } =
    scriptPieces
        |> List.map scriptPieceView
        |> Element.textColumn [ Element.spacing 5, Element.padding 40 ]


scriptPieceView : ScriptPiece -> Element Msg
scriptPieceView scriptPiece =
    case scriptPiece of
        UnsurePiece u ->
            Element.paragraph []
                [ Element.text "Unsure"
                , iconWrapper Material.Icons.done
                , Element.text u
                ]

        _ ->
            Element.none


iconWrapper : Material.Icons.Types.Icon Msg -> Element Msg
iconWrapper icon =
    Widget.Icon.elmMaterialIcons Color icon { size = 20, color = Color.blue }



--  ____            _       _     ____                _
-- / ___|  ___ _ __(_)_ __ | |_  |  _ \ __ _ _ __ ___(_)_ __   __ _
-- \___ \ / __| '__| | '_ \| __| | |_) / _` | '__/ __| | '_ \ / _` |
--  ___) | (__| |  | | |_) | |_  |  __/ (_| | |  \__ \ | | | | (_| |
-- |____/ \___|_|  |_| .__/ \__| |_|   \__,_|_|  |___/_|_| |_|\__, |
--                   |_|                                      |___/


type ScriptPiece
    = UnsurePiece String
    | IgnorePiece String
    | CharacterPiece String
    | LinePiece String
    | StageDirectionPiece String


scriptPiecesFromPlainScript : String -> List ScriptPiece
scriptPiecesFromPlainScript plain =
    plain
        |> String.split "\n"
        |> List.map UnsurePiece


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
            Err "Parse parseScriptHelper ended unexpectedly on StartingParse"



--  ____            _       _     _____                       _
-- / ___|  ___ _ __(_)_ __ | |_  | ____|_  ___ __   ___  _ __| |_
-- \___ \ / __| '__| | '_ \| __| |  _| \ \/ / '_ \ / _ \| '__| __|
--  ___) | (__| |  | | |_) | |_  | |___ >  <| |_) | (_) | |  | |_
-- |____/ \___|_|  |_| .__/ \__| |_____/_/\_\ .__/ \___/|_|   \__|
--                   |_|                    |_|


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
