module Main exposing (main)

import Base64
import Color
import Element exposing (Element)
import Element.Font
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import Json.Encode
import Macbeth exposing (scene1)
import Material.Icons exposing (offline_bolt)
import Material.Icons.Types exposing (Coloring(..))
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Platform
import Widget
import Widget.Icon exposing (Icon)
import Widget.Material as Material exposing (defaultPalette)



--     _
--    / \   _ __  _ __
--   / _ \ | '_ \| '_ \
--  / ___ \| |_) | |_) |
-- /_/   \_\ .__/| .__/
--         |_|   |_|
-- App: Users paste a script in, then mark which script parts are character names etc, then export to the app
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


init : ( Model, Cmd Msg )
init =
    ( { plainScript = scene1
      , scriptPieces = scriptPiecesFromPlainScript scene1
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change s ->
            ( { model
                | plainScript = s
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
-- Script interface: A user can go through what they've copy-pasted and mark parts


palette =
    { defaultPalette | primary = Color.black }


scriptParseApp : Model -> { title : String, body : List (Element Msg) }
scriptParseApp model =
    { title = "Script Parser"
    , body =
        [ Element.column [ Element.width Element.fill ]
            [ Element.el [ Element.width Element.fill ] (Element.text "Parse state")
            , Element.row
                [ Element.width Element.fill ]
                [ if model.plainScript /= "" then
                    Element.el [ Element.width Element.fill, Element.alignTop ] <| scriptPiecesView model

                  else
                    Element.none
                , Element.el [ Element.width Element.fill, Element.alignTop ] <| scriptEditor model
                ]
            ]
        ]
    }


scriptEditor : Model -> Element Msg
scriptEditor { plainScript } =
    Element.el [ Element.width Element.fill, Element.Font.size 10 ] <|
        Element.Input.multiline []
            { onChange = Change
            , text = plainScript
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Paste a script here to parse into cues!"))
            , label = Element.Input.labelAbove [] <| Element.text ""
            , spellcheck = False
            }


scriptPiecesView : Model -> Element Msg
scriptPiecesView { scriptPieces } =
    scriptPieces
        |> List.map scriptPieceView
        |> Element.textColumn [ Element.spacing 5, Element.padding 20 ]


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
-- Script Parsing: Do we have enough info about this script to export it to the app?


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
