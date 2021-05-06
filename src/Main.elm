module Main exposing (main)

import Browser.Dom
import Browser.Navigation as Nav
import Color
import Element exposing (DeviceClass(..), Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import Examples exposing (scene1)
import Html.Attributes
import Html.Events
import Json.Decode as D
import List.Extra
import Material.Icons exposing (offline_bolt)
import Material.Icons.Types exposing (Coloring(..))
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Platform
import Scripts
    exposing
        ( ScriptPiece(..)
        , ScriptPieceKind(..)
        , cueCannonUrl
        , extractPlainScript
        , makeScriptPieces
        , parseScript
        )
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
    | ChangeScript (List ScriptPiece) String
    | Export String
    | ChangeScriptPiece ScriptPieceKind
    | SetScriptPieces (List ScriptPiece)
    | LoadedScriptPieces (List ScriptPiece)
    | ReplaceScriptPiecesWithLoaded
      -- View actions
    | SelectPiece Int
    | NextError
    | LabelMouseEnter Int
    | LabelMouseLeave
    | ShortcutPressed Int


type EditingProgress
    = JustStarting
    | EditingScript String (List ScriptPiece)
    | SplittingScript (List ScriptPiece)
    | DoneEditingScript (List ScriptPiece) Scripts.Script


type alias Model =
    -- Script data
    { editingProgress : EditingProgress
    , loadedScriptPieces : List ScriptPiece

    -- View data
    , selectedPiece : Maybe Int
    , labelMouseOver : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( { editingProgress = JustStarting
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
            case m.editingProgress of
                SplittingScript scriptPieces ->
                    ( { m
                        | selectedPiece =
                            List.Extra.findIndex
                                (\(ScriptPiece kind _) -> kind == UnsurePiece)
                                scriptPieces
                      }
                    , cmd
                    )

                _ ->
                    ( m, cmd )

        changingSelectedPieceTo : ScriptPieceKind -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
        changingSelectedPieceTo k ( m, cmd ) =
            case m.editingProgress of
                SplittingScript scriptPieces ->
                    let
                        changeScriptPieceType (ScriptPiece _ line) =
                            ScriptPiece k line

                        newScriptPieces =
                            case m.selectedPiece of
                                Just i ->
                                    scriptPieces
                                        |> List.Extra.updateAt i changeScriptPieceType

                                Nothing ->
                                    scriptPieces
                    in
                    ( { m
                        | editingProgress = SplittingScript newScriptPieces
                      }
                    , Cmd.batch (storeScriptPieces newScriptPieces :: [ cmd ])
                    )

                _ ->
                    ( m, cmd )
    in
    case msg of
        -- Editing script and script pieces
        Export href ->
            ( model, Nav.load href )

        ChangeScript ll s ->
            case model.editingProgress of
                EditingScript _ l ->
                    ( { model | editingProgress = EditingScript s l }, Cmd.none )

                _ ->
                    ( { model | editingProgress = EditingScript s ll }, Cmd.none )

        ChangeScriptPiece newKind ->
            ( model, Cmd.none )
                |> changingSelectedPieceTo newKind
                |> selectingNextError

        SetScriptPieces pieces ->
            ( { model | editingProgress = SplittingScript pieces }, Cmd.none )

        LoadedScriptPieces pieces ->
            ( { model | loadedScriptPieces = pieces }, Cmd.none )

        ReplaceScriptPiecesWithLoaded ->
            ( { model | editingProgress = SplittingScript model.loadedScriptPieces }, Cmd.none )

        -- UI Changes
        LabelMouseEnter i ->
            ( { model | labelMouseOver = Just i }, Cmd.none )

        LabelMouseLeave ->
            ( { model | labelMouseOver = Nothing }, Cmd.none )

        SelectPiece i ->
            case model.editingProgress of
                SplittingScript scriptPieces ->
                    if List.length scriptPieces > i && i >= 0 then
                        ( { model | selectedPiece = Just i }, setShortcutFocus )

                    else
                        ( model, setShortcutFocus )

                _ ->
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
        [ Element.column [ fillWidth ] <|
            [ topBar model.editingProgress
            , case model.editingProgress of
                JustStarting ->
                    loaders "" model.loadedScriptPieces

                EditingScript plainScript _ ->
                    loaders plainScript model.loadedScriptPieces

                SplittingScript scriptPieces ->
                    scriptPieces
                        |> List.indexedMap (scriptPieceView model.selectedPiece model.labelMouseOver)
                        |> Element.textColumn
                            ([ Element.spacing 5, Element.padding 20 ] ++ keyboardShortcutListenerAttributes)

                DoneEditingScript scriptPieces exportLink ->
                    scriptPieces
                        |> List.indexedMap (scriptPieceView model.selectedPiece model.labelMouseOver)
                        |> Element.textColumn
                            ([ Element.spacing 5, Element.padding 20 ] ++ keyboardShortcutListenerAttributes)
            ]
        ]
    }


topBar : EditingProgress -> Element Msg
topBar progress =
    let
        firstButton =
            { icon = Material.Icons.book |> Widget.Icon.elmMaterialIcons Color
            , text = "Select script"
            , onPress = Just NoOp
            }

        secondButton =
            { icon = Material.Icons.edit |> Widget.Icon.elmMaterialIcons Color
            , text = "Split script into cues"
            , onPress = Nothing
            }

        exportButton =
            { icon =
                Material.Icons.cancel |> Widget.Icon.elmMaterialIcons Color
            , text = "Open script in app"
            , onPress = Nothing
            }

        arrow =
            { icon = Material.Icons.arrow_right |> Widget.Icon.elmMaterialIcons Color
            , text = ""
            , onPress = Nothing
            }
    in
    buttonWrapper [] <|
        case progress of
            JustStarting ->
                [ firstButton, arrow, secondButton, arrow, exportButton ]

            EditingScript plainScript oldScriptPieces ->
                let
                    splitScript =
                        Just <| SetScriptPieces <| makeScriptPieces plainScript oldScriptPieces
                in
                [ firstButton, arrow, { secondButton | onPress = splitScript }, arrow, exportButton ]

            SplittingScript scriptPieces ->
                let
                    plainScript =
                        extractPlainScript scriptPieces
                in
                [ { firstButton | text = "Edit script", onPress = Just (ChangeScript scriptPieces plainScript) }
                , arrow
                , secondButton
                , arrow
                , exportButton
                ]

            DoneEditingScript scriptPieces exportLink ->
                [ firstButton
                , arrow
                , secondButton
                , arrow
                , { icon =
                        Material.Icons.upgrade |> Widget.Icon.elmMaterialIcons Color
                  , text = "Open script in app"
                  , onPress = Just (Export (cueCannonUrl exportLink))
                  }
                ]


loaders : String -> List ScriptPiece -> Element Msg
loaders plainScript loadedScriptPieces =
    let
        exampleLoader =
            Element.row [ Element.paddingXY 20 0 ] [ Widget.textButton (Material.textButton palette) { onPress = Nothing, text = "Macbeth" } ]

        localStorageLoader =
            Element.el [ Element.paddingXY 20 0 ] <|
                case loadedScriptPieces of
                    [] ->
                        Widget.textButton (Material.textButton palette) { onPress = Nothing, text = "No saved script found" }

                    _ ->
                        Widget.textButton (Material.textButton palette)
                            { onPress = Just ReplaceScriptPiecesWithLoaded
                            , text = "Load"
                            }

        loaderView label loader =
            Element.row
                [ fillWidth
                , Element.Border.widthEach { bottom = 0, left = 0, right = 0, top = 1 }
                , Element.Border.solid
                , Element.Border.color (Element.rgb 0.8 0.8 0.8)
                , Element.paddingXY 10 30
                ]
                [ Element.el [ scaledFont 2, Element.Font.heavy ] (Element.text label)
                , loader
                ]
    in
    Element.column
        [ fillWidth, Element.alignTop, Element.padding 50, Element.spacing 20 ]
        [ Element.el [ scaledFont 3 ] (Element.text "Load a script from...")
        , loaderView "Copy/paste" (copyPasteLoader plainScript)
        , loaderView "Examples" exampleLoader
        , loaderView "Previously saved script" localStorageLoader
        ]


copyPasteLoader : String -> Element Msg
copyPasteLoader plainScript =
    Element.el
        [ fillWidth
        , scaledFont 1
        , Element.alignRight
        , Element.alignTop
        , Element.paddingXY 20 0
        ]
    <|
        Element.Input.multiline []
            { onChange = ChangeScript []
            , text = plainScript
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Paste here!"))
            , label = Element.Input.labelHidden "Copy/paste"
            , spellcheck = False
            }


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
    , title =
        Element.row [ Element.padding 8, Element.spacing 4 ]
            [ Widget.Icon.elmMaterialIcons Color Material.Icons.question_answer <|
                { size = 20, color = palette.on.primary }
            , Element.text "Cue Maker"
            ]
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



--  _____ _             _   _ ___               _   _
-- | ____| |_ __ ___   | | | |_ _|  _ __   __ _| |_| |_ ___ _ __ _ __  ___
-- |  _| | | '_ ` _ \  | | | || |  | '_ \ / _` | __| __/ _ \ '__| '_ \/ __|
-- | |___| | | | | | | | |_| || |  | |_) | (_| | |_| ||  __/ |  | | | \__ \
-- |_____|_|_| |_| |_|  \___/|___| | .__/ \__,_|\__|\__\___|_|  |_| |_|___/
--                                 |_|


scale =
    Element.modular 16 1.25


scaledFont s =
    Element.Font.size (round (scale s))


fillWidth =
    Element.width Element.fill
