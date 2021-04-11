module Main exposing (main)

import Browser.Navigation as Nav
import Color
import Element exposing (DeviceClass(..), Element)
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import List.Extra
import Macbeth exposing (scene1)
import Material.Icons exposing (offline_bolt)
import Material.Icons.Types exposing (Coloring(..))
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Platform
import ScriptExport exposing (ScriptPiece(..), ScriptPieceKind(..), cueCannonUrl, parseScript, scriptPiecesFromPlainScript)
import Storage exposing (loadPlainScript, storePlainScript, storeScriptPieces)
import Widget
import Widget.Icon exposing (Icon)
import Widget.Material as Material exposing (defaultPalette)
import Widget.Material.Color



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


type
    Msg
    -- Script actions
    = ChangeScript String
    | Export String
    | ChangeScriptPiece ScriptPieceKind
      -- View actions
    | SelectPiece Int
    | NextError
    | LabelMouseEnter Int
    | LabelMouseLeave


type alias Model =
    -- Script data
    { plainScript : String
    , scriptPieces : List ScriptPiece

    -- View data
    , selectedPiece : Maybe Int
    , labelMouseOver : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { plainScript = scene1
      , scriptPieces = scriptPiecesFromPlainScript scene1
      , selectedPiece = Nothing
      , labelMouseOver = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- Editing script and script pieces
        Export href ->
            ( model, Nav.load href )

        ChangeScript s ->
            let
                newScriptPieces =
                    -- FIXME overwrites script pieces, implement merge
                    scriptPiecesFromPlainScript s
            in
            ( { model | plainScript = s, scriptPieces = newScriptPieces }
            , Cmd.batch
                [ storePlainScript s
                , storeScriptPieces newScriptPieces
                ]
            )

        ChangeScriptPiece newKind ->
            case model.selectedPiece of
                Just i ->
                    let
                        changeScriptPieceType (ScriptPiece _ line) =
                            ScriptPiece newKind line

                        newScriptPieces =
                            model.scriptPieces
                                |> List.Extra.updateAt i changeScriptPieceType
                    in
                    ( { model | scriptPieces = newScriptPieces }, storeScriptPieces newScriptPieces )

                Nothing ->
                    ( model, Cmd.none )

        -- UI Changes
        LabelMouseEnter i ->
            ( { model | labelMouseOver = Just i }, Cmd.none )

        LabelMouseLeave ->
            ( { model | labelMouseOver = Nothing }, Cmd.none )

        SelectPiece i ->
            if List.length model.scriptPieces > i && i >= 0 then
                ( { model | selectedPiece = Just i }, Cmd.none )

            else
                ( model, Cmd.none )

        NextError ->
            case List.Extra.findIndex (\(ScriptPiece kind _) -> kind == UnsurePiece) model.scriptPieces of
                Just piece ->
                    ( { model | selectedPiece = Just piece }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


subscriptions : Metadata -> pages -> Model -> Sub Msg
subscriptions _ _ _ =
    loadPlainScript ChangeScript



--  ____            _       _     _       _             __
-- / ___|  ___ _ __(_)_ __ | |_  (_)_ __ | |_ ___ _ __ / _| __ _  ___ ___
-- \___ \ / __| '__| | '_ \| __| | | '_ \| __/ _ \ '__| |_ / _` |/ __/ _ \
--  ___) | (__| |  | | |_) | |_  | | | | | ||  __/ |  |  _| (_| | (_|  __/
-- |____/ \___|_|  |_| .__/ \__| |_|_| |_|\__\___|_|  |_|  \__,_|\___\___|
--                   |_|
-- Script interface: A user can go through what they've copy-pasted and mark parts


scriptParseApp : Model -> { title : String, body : List (Element Msg) }
scriptParseApp model =
    { title = "CueCannon - Script Parser"
    , body =
        [ Element.column [ Element.width Element.fill ]
            [ topBar model
            , Element.row
                [ Element.width Element.fill ]
                [ if model.plainScript /= "" then
                    scriptPiecesView model

                  else
                    Element.none
                , scriptEditor model
                ]
            ]
        ]
    }


topBar : Model -> Element Msg
topBar model =
    let
        ( leftButtons, rightButtons ) =
            case parseScript model.scriptPieces of
                Err s ->
                    ( scriptPieceButtons
                    , [ { icon =
                            Material.Icons.bug_report
                                |> Widget.Icon.elmMaterialIcons Color
                        , text = "Errors found: fix before export"
                        , onPress = Just NextError
                        }
                      ]
                    )

                Ok href ->
                    ( []
                    , [ { icon =
                            Material.Icons.upgrade
                                |> Widget.Icon.elmMaterialIcons Color
                        , text = "Open script in app"
                        , onPress = Just (Export (cueCannonUrl href))
                        }
                      ]
                    )
    in
    if model.plainScript == "" then
        Element.none

    else
        buttonWrapper leftButtons rightButtons


scriptEditor : Model -> Element Msg
scriptEditor { plainScript } =
    let
        ( fontSize, width ) =
            if plainScript /= "" then
                ( 12, Element.px 350 )

            else
                ( 18, Element.fill )
    in
    Element.el
        [ Element.width width
        , Element.alignRight
        , Element.alignTop
        , Element.Font.size fontSize
        , Element.paddingXY 10 0
        ]
    <|
        Element.Input.multiline []
            { onChange = ChangeScript
            , text = plainScript
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Paste a script here to parse into cues!"))
            , label = Element.Input.labelAbove [] <| Element.text ""
            , spellcheck = False
            }


scriptPiecesView : Model -> Element Msg
scriptPiecesView { scriptPieces, selectedPiece, labelMouseOver } =
    scriptPieces
        |> List.indexedMap (scriptPieceView selectedPiece labelMouseOver)
        |> Element.textColumn [ Element.spacing 5, Element.padding 20 ]


scriptPieceView : Maybe Int -> Maybe Int -> Int -> ScriptPiece -> Element Msg
scriptPieceView selectedPieceIndex labelIndex index (ScriptPiece kind line) =
    let
        style =
            Element.spacing 60
                :: Element.pointer
                :: Element.mouseOver [ Element.scale 1.01 ]
                :: Element.Events.onClick (SelectPiece index)
                :: (if selectedPieceIndex == Just index then
                        Widget.Material.Color.textAndBackground
                            (Widget.Material.Color.fromCIELCH { l = 94, c = 50, h = 83 })

                    else
                        []
                   )

        labelHelper =
            Element.text (labelFromScriptPiece kind)

        mouseOverHelper =
            Element.Events.onMouseEnter (LabelMouseEnter index)
                :: Element.Events.onMouseLeave LabelMouseLeave
                :: (if labelIndex == Just index then
                        [ Element.onLeft labelHelper ]

                    else
                        []
                   )

        iconHelper =
            Element.el mouseOverHelper <|
                Widget.Icon.elmMaterialIcons Color (iconFromScriptPiece kind) <|
                    { size = 20, color = colorFromScriptPiece kind }

        viewHelper scriptLine =
            Element.row style
                [ Element.paragraph [] [ Element.text scriptLine ]
                , iconHelper

                -- , Element.text (String.fromInt (index + 1))
                ]
    in
    viewHelper line



--  _   _      _                                       _
-- | | | | ___| |_ __   ___ _ __ ___    __ _ _ __   __| |
-- | |_| |/ _ \ | '_ \ / _ \ '__/ __|  / _` | '_ \ / _` |
-- |  _  |  __/ | |_) |  __/ |  \__ \ | (_| | | | | (_| |
-- |_| |_|\___|_| .__/ \___|_|  |___/  \__,_|_| |_|\__,_|
--              |_|
--  _   _ _   _ _ _ _   _
-- | | | | |_(_) (_) |_(_) ___  ___
-- | | | | __| | | | __| |/ _ \/ __|
-- | |_| | |_| | | | |_| |  __/\__ \
--  \___/ \__|_|_|_|\__|_|\___||___/
--


palette =
    { defaultPalette | primary = Color.black }


allScriptPieceKinds : List ScriptPieceKind
allScriptPieceKinds =
    [ UnsurePiece, CharacterPiece, LinePiece, IgnorePiece, StageDirectionPiece, TitlePiece ]


scriptPieceButtons =
    allScriptPieceKinds
        |> List.map
            (\x ->
                { icon = iconFromScriptPiece x |> Widget.Icon.elmMaterialIcons Color
                , text = labelFromScriptPiece x
                , onPress = Just (ChangeScriptPiece x)
                }
            )


iconFromScriptPiece kind =
    case kind of
        UnsurePiece ->
            Material.Icons.dangerous

        CharacterPiece ->
            Material.Icons.face

        LinePiece ->
            Material.Icons.receipt

        IgnorePiece ->
            Material.Icons.border_clear

        StageDirectionPiece ->
            Material.Icons.directions

        TitlePiece ->
            Material.Icons.grading


labelFromScriptPiece kind =
    case kind of
        UnsurePiece ->
            "Unsure"

        CharacterPiece ->
            "Character"

        LinePiece ->
            "Line"

        IgnorePiece ->
            "Ignore"

        StageDirectionPiece ->
            "Stage Direction"

        TitlePiece ->
            "Title"


colorFromScriptPiece kind =
    case kind of
        UnsurePiece ->
            palette.error

        CharacterPiece ->
            palette.primary

        LinePiece ->
            palette.primary

        IgnorePiece ->
            palette.on.error

        StageDirectionPiece ->
            palette.on.error

        TitlePiece ->
            palette.primary


buttonWrapper leftButtons rightButtons =
    Widget.buttonBar (Material.buttonBar leftButtons palette) (barConfig rightButtons)


barConfig actions =
    { deviceClass = Desktop
    , openLeftSheet = Nothing
    , openRightSheet = Nothing
    , openTopSheet = Nothing
    , primaryActions = actions
    , search = Nothing
    , title = Element.text "Hello"
    }
