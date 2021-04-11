module Main exposing (main)

import Browser.Dom
import Browser.Navigation as Nav
import Color
import Element exposing (DeviceClass(..), Element)
import Element.Background
import Element.Events
import Element.Font
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import Html.Attributes
import Html.Events
import Json.Decode as D
import List.Extra
import Macbeth exposing (scene1)
import Material.Icons exposing (offline_bolt)
import Material.Icons.Types exposing (Coloring(..))
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Platform
import ScriptExport exposing (ScriptPiece(..), ScriptPieceKind(..), cueCannonUrl, parseScript, scriptPiecesFromPlainScript)
import Storage exposing (decodeScriptPieces, loadScriptPieces, storeScriptPieces)
import Task
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


type Msg
    = NoOp
      -- Script actions
    | ChangeScript String
    | Export String
    | ChangeScriptPiece ScriptPieceKind
    | LoadedScriptPieces (List ScriptPiece)
    | ReplaceScriptPiecesWithLoaded
      -- View actions
    | SelectPiece Int
    | NextError
    | LabelMouseEnter Int
    | LabelMouseLeave
    | ShortcutPressed Int


type alias Model =
    -- Script data
    { plainScript : String
    , scriptPieces : List ScriptPiece
    , loadedScriptPieces : List ScriptPiece

    -- View data
    , selectedPiece : Maybe Int
    , labelMouseOver : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { plainScript = scene1
      , scriptPieces = scriptPiecesFromPlainScript scene1
      , loadedScriptPieces = []
      , selectedPiece = Nothing
      , labelMouseOver = Nothing
      }
    , setShortcutFocus
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        selectingNextError : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
        selectingNextError ( m, cmd ) =
            ( { m
                | selectedPiece =
                    List.Extra.findIndex
                        (\(ScriptPiece kind _) -> kind == UnsurePiece)
                        m.scriptPieces
              }
            , cmd
            )

        changingSelectedPieceTo : ScriptPieceKind -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
        changingSelectedPieceTo k ( m, cmd ) =
            let
                changeScriptPieceType (ScriptPiece _ line) =
                    ScriptPiece k line

                newScriptPieces =
                    case m.selectedPiece of
                        Just i ->
                            m.scriptPieces
                                |> List.Extra.updateAt i changeScriptPieceType

                        Nothing ->
                            m.scriptPieces
            in
            ( { m
                | scriptPieces = newScriptPieces
              }
            , Cmd.batch (storeScriptPieces newScriptPieces :: [ cmd ])
            )
    in
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
            , Cmd.none
              -- storeScriptPieces newScriptPieces
            )

        ChangeScriptPiece newKind ->
            ( model, Cmd.none )
                |> changingSelectedPieceTo newKind
                |> selectingNextError

        LoadedScriptPieces pieces ->
            ( { model | loadedScriptPieces = pieces }, Cmd.none )

        ReplaceScriptPiecesWithLoaded ->
            ( { model | scriptPieces = model.loadedScriptPieces }, Cmd.none )

        -- UI Changes
        LabelMouseEnter i ->
            ( { model | labelMouseOver = Just i }, Cmd.none )

        LabelMouseLeave ->
            ( { model | labelMouseOver = Nothing }, Cmd.none )

        SelectPiece i ->
            if List.length model.scriptPieces > i && i >= 0 then
                ( { model | selectedPiece = Just i }, setShortcutFocus )

            else
                ( model, setShortcutFocus )

        NextError ->
            ( model, Cmd.none )
                |> selectingNextError

        ShortcutPressed i ->
            case i of
                85 ->
                    ( model, Cmd.none )
                        |> changingSelectedPieceTo UnsurePiece
                        |> selectingNextError

                67 ->
                    ( model, Cmd.none )
                        |> changingSelectedPieceTo CharacterPiece
                        |> selectingNextError

                76 ->
                    ( model, Cmd.none )
                        |> changingSelectedPieceTo LinePiece
                        |> selectingNextError

                73 ->
                    ( model, Cmd.none )
                        |> changingSelectedPieceTo IgnorePiece
                        |> selectingNextError

                83 ->
                    ( model, Cmd.none )
                        |> changingSelectedPieceTo StageDirectionPiece
                        |> selectingNextError

                84 ->
                    ( model, Cmd.none )
                        |> changingSelectedPieceTo TitlePiece
                        |> selectingNextError

                _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Metadata -> pages -> Model -> Sub Msg
subscriptions _ _ _ =
    loadScriptPieces
        (\value ->
            case D.decodeValue decodeScriptPieces value of
                Ok pieces ->
                    LoadedScriptPieces pieces

                Err _ ->
                    NoOp
        )



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
        [ Element.column [ Element.width Element.fill ] <|
            [ topBar model
            , Element.row
                [ Element.width Element.fill ]
                [ scriptPiecesView model, loaders model ]
            ]
        ]
    }


topBar : Model -> Element Msg
topBar model =
    let
        errorCount =
            model.scriptPieces
                |> List.filter (\(ScriptPiece kind _) -> kind == UnsurePiece)
                |> List.length

        ( exportIcon, exportMsg ) =
            case parseScript model.scriptPieces of
                Err _ ->
                    ( Material.Icons.cancel, Nothing )

                Ok href ->
                    ( Material.Icons.upgrade, Just (Export (cueCannonUrl href)) )

        exportButton =
            { icon =
                exportIcon |> Widget.Icon.elmMaterialIcons Color
            , text = "Open script in app"
            , onPress = exportMsg
            }
    in
    buttonWrapper [ exportButton ] []


loaders : Model -> Element Msg
loaders model =
    Element.column [ Element.alignTop, Element.paddingXY 0 20 ] <|
        Element.el [ Element.Font.size 16 ]
            (Element.text "Loaded from Copy/Paste")
            :: scriptEditor model
            :: (case model.loadedScriptPieces of
                    [] ->
                        []

                    _ ->
                        [ Element.el
                            [ Element.Font.size 16
                            , Element.Events.onClick ReplaceScriptPiecesWithLoaded
                            ]
                            (Element.text "Load from save")
                        ]
               )


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
        |> Element.textColumn
            ([ Element.spacing 5
             , Element.padding 20
             ]
                ++ keyboardShortcutListenerAttributes
            )


scriptPieceView : Maybe Int -> Maybe Int -> Int -> ScriptPiece -> Element Msg
scriptPieceView selectedPieceIndex labelIndex index (ScriptPiece kind line) =
    let
        isSelected =
            selectedPieceIndex == Just index

        isHovered =
            labelIndex == Just index

        style =
            Element.spacing 60
                :: Element.pointer
                :: Element.mouseOver [ Element.scale 1.01 ]
                :: (if isSelected then
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
                :: (if isHovered then
                        [ Element.onLeft labelHelper ]

                    else
                        []
                   )

        iconHelper =
            Element.el mouseOverHelper <|
                Widget.Icon.elmMaterialIcons Color (iconFromScriptPiece kind) <|
                    { size = 20, color = colorFromScriptPiece kind }

        scriptPieceKindSelector =
            if isSelected then
                allScriptPieceKinds
                    |> List.map
                        (\k ->
                            let
                                label =
                                    labelFromScriptPiece k

                                initial =
                                    String.slice 0 1 label |> String.toLower
                            in
                            Element.el
                                [ Element.padding 3
                                , Element.Font.size 14
                                , Element.Events.onClick (ChangeScriptPiece k)
                                ]
                            <|
                                Element.text (label ++ " (" ++ initial ++ ")")
                        )
                    |> Element.paragraph []
                    |> List.singleton

            else
                []

        viewHelper scriptLine =
            Element.row style
                [ Element.textColumn []
                    (Element.paragraph
                        [ Element.Events.onClick (SelectPiece index)
                        ]
                        [ Element.text scriptLine ]
                        :: scriptPieceKindSelector
                    )
                , iconHelper
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
    [ CharacterPiece, LinePiece, StageDirectionPiece, TitlePiece, IgnorePiece, UnsurePiece ]


scriptPieceButtons =
    let
        makeButton kind =
            { icon = iconFromScriptPiece kind |> Widget.Icon.elmMaterialIcons Color
            , text = labelFromScriptPiece kind
            , onPress = Just (ChangeScriptPiece kind)
            }
    in
    List.map makeButton allScriptPieceKinds


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
            palette.primary

        StageDirectionPiece ->
            palette.primary

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


keyboardShortcutListenerAttributes =
    -- Must be focusable
    [ Element.htmlAttribute (Html.Attributes.tabindex 0)

    -- Must track id to set focus
    , Element.htmlAttribute (Html.Attributes.id "scriptPieces")

    -- Events are on the element
    , Element.htmlAttribute (Html.Events.on "keydown" (D.map ShortcutPressed Html.Events.keyCode))
    ]


setShortcutFocus =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus "scriptPieces")
