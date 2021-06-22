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
import Element.Lazy
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import Examples exposing (book, scene1)
import File exposing (File)
import File.Select as Select
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
import Transcription exposing (Transcription, renderTranscription, transcriptionDecoder)
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
        , view = view scriptParseApp scriptParseTopBar
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
    | EditTitle String
    | DoneEditingTitle
    | RequestAwsFile
    | SelectAwsFile File
    | LoadAwsFile String


type EditingProgress
    = JustStarting
    | EditingScript String (List ScriptPiece)
    | SplittingScript (List ScriptPiece)
    | DoneEditingScript (List ScriptPiece) Scripts.Script


type Title
    = Untitled
    | EditingTitle String String
    | Titled String


type alias Model =
    -- Script data
    { editingProgress : EditingProgress
    , loadedScriptPieces : List ScriptPiece

    -- View data
    , selectedPiece : Int
    , labelMouseOver : Maybe Int
    , parseError : Maybe String
    , title : Title
    }


init : ( Model, Cmd Msg )
init =
    ( { editingProgress = JustStarting
      , loadedScriptPieces = []
      , selectedPiece = 0
      , labelMouseOver = Nothing
      , parseError = Nothing
      , title = Untitled
      }
    , setShortcutFocus
    )


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



--  _   _           _       _                 _          _
-- | | | |_ __   __| | __ _| |_ ___     _    | |__   ___| |_ __   ___ _ __ ___
-- | | | | '_ \ / _` |/ _` | __/ _ \  _| |_  | '_ \ / _ \ | '_ \ / _ \ '__/ __|
-- | |_| | |_) | (_| | (_| | ||  __/ |_   _| | | | |  __/ | |_) |  __/ |  \__ \
--  \___/| .__/ \__,_|\__,_|\__\___|   |_|   |_| |_|\___|_| .__/ \___|_|  |___/
--       |_|                                              |_|


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
                |> changingSelectedPieceKindTo newKind
                |> selectingNextError
                |> checkingParser

        SetScriptPieces pieces ->
            ( { model | editingProgress = SplittingScript pieces }, Cmd.none )
                |> checkingParser

        LoadedScriptPieces pieces ->
            ( { model | loadedScriptPieces = pieces }, Cmd.none )

        ReplaceScriptPiecesWithLoaded ->
            ( { model
                | editingProgress = SplittingScript model.loadedScriptPieces
              }
            , Cmd.none
            )
                |> checkingParser

        -- UI Changes
        LabelMouseEnter i ->
            ( { model | labelMouseOver = Just i }, Cmd.none )

        LabelMouseLeave ->
            ( { model | labelMouseOver = Nothing }, Cmd.none )

        SelectPiece i ->
            case progressIfEditing model of
                Just scriptPieces ->
                    if List.length scriptPieces > i && i >= 0 then
                        ( { model | selectedPiece = i }, setShortcutFocus )

                    else
                        ( model, setShortcutFocus )

                Nothing ->
                    ( model, setShortcutFocus )

        NextError ->
            ( model, Cmd.none )
                |> selectingNextError

        EditTitle newTitle ->
            let
                oldTitle =
                    case model.title of
                        Titled t ->
                            t

                        _ ->
                            "Untitled"
            in
            ( { model | title = EditingTitle oldTitle newTitle }, Cmd.none )

        DoneEditingTitle ->
            case progressIfEditing model of
                Just scriptPieces ->
                    ( model, storeScriptPieces scriptPieces )
                        |> finishingTitleEdit

                Nothing ->
                    ( model, Cmd.none )

        ShortcutPressed i ->
            case i of
                85 ->
                    -- U
                    ( model, Cmd.none )
                        |> changingSelectedPieceKindTo UnsurePiece
                        |> selectingNextError
                        |> checkingParser

                67 ->
                    -- C
                    ( model, Cmd.none )
                        |> changingSelectedPieceKindTo CharacterPiece
                        |> selectingNextError
                        |> checkingParser

                76 ->
                    -- L
                    ( model, Cmd.none )
                        |> changingSelectedPieceKindTo LinePiece
                        |> selectingNextError
                        |> checkingParser

                73 ->
                    -- I
                    ( model, Cmd.none )
                        |> changingSelectedPieceKindTo IgnorePiece
                        |> selectingNextError
                        |> checkingParser

                83 ->
                    -- S
                    ( model, Cmd.none )
                        |> changingSelectedPieceKindTo StageDirectionPiece
                        |> selectingNextError
                        |> checkingParser

                84 ->
                    -- T
                    ( model, Cmd.none )
                        |> changingSelectedPieceKindTo TitlePiece
                        |> selectingNextError
                        |> checkingParser

                74 ->
                    -- J (like vim)
                    let
                        maxSelectedPieceIndex =
                            case model.editingProgress of
                                JustStarting ->
                                    0

                                EditingScript _ scriptPieces ->
                                    List.length scriptPieces - 1

                                SplittingScript scriptPieces ->
                                    List.length scriptPieces - 1

                                DoneEditingScript scriptPieces _ ->
                                    List.length scriptPieces - 1
                    in
                    ( { model
                        | selectedPiece =
                            min
                                maxSelectedPieceIndex
                                (model.selectedPiece + 1)
                      }
                    , Cmd.none
                    )

                75 ->
                    -- K (like vim)
                    ( { model | selectedPiece = max 0 (model.selectedPiece - 1) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RequestAwsFile ->
            ( model, Select.file [ "text/json" ] SelectAwsFile )

        SelectAwsFile file ->
            ( model, Task.perform LoadAwsFile (File.toString file) )

        LoadAwsFile t ->
            case D.decodeString transcriptionDecoder t of
                Ok transcription ->
                    ( { model
                        | editingProgress =
                            transcription
                                |> renderTranscription
                                |> makeScriptPieces []
                                |> SplittingScript
                      }
                    , Cmd.none
                    )
                        |> checkingParser

                Err e ->
                    ( { model | parseError = Just <| D.errorToString e }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )


finishingTitleEdit : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
finishingTitleEdit ( m, cmd ) =
    let
        newTitle =
            case m.title of
                Titled t ->
                    Titled t

                EditingTitle old new ->
                    if String.trim new == "" then
                        Titled old

                    else
                        Titled new

                Untitled ->
                    Untitled
    in
    ( { m | title = newTitle }, cmd )


selectingNextError : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
selectingNextError ( m, cmd ) =
    case progressIfEditing m of
        Just scriptPieces ->
            let
                scriptPieceIndex =
                    List.Extra.findIndex
                        (\(ScriptPiece kind _) -> kind == UnsurePiece)
                        scriptPieces
            in
            ( { m
                | selectedPiece = scriptPieceIndex |> Maybe.withDefault 0
              }
            , cmd
              -- FIXME Removing until bugfix. Wrapped script lines make scrolling go offscreen
              -- , Cmd.batch [ cmd, scrollToScriptPiece scriptPieceIndex ]
            )

        Nothing ->
            ( m, cmd )


changingSelectedPieceKindTo : ScriptPieceKind -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
changingSelectedPieceKindTo k ( m, cmd ) =
    let
        changePiece scriptPieces =
            let
                changeScriptPieceType (ScriptPiece _ line) =
                    ScriptPiece k line

                newScriptPieces =
                    scriptPieces
                        |> List.Extra.updateAt m.selectedPiece changeScriptPieceType
            in
            ( { m | editingProgress = SplittingScript newScriptPieces }
            , Cmd.batch (storeScriptPieces newScriptPieces :: [ cmd ])
            )
    in
    case progressIfEditing m of
        Just scriptPieces ->
            changePiece scriptPieces
                |> settingTitleFromSelected

        _ ->
            ( m, cmd )


checkingParser : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkingParser ( m, cmd ) =
    case progressIfEditing m of
        Just scriptPieces ->
            let
                anyUnsurePieces =
                    List.any (\(ScriptPiece kind _) -> kind == UnsurePiece) scriptPieces

                title =
                    case m.title of
                        Titled t ->
                            Just t

                        _ ->
                            Nothing
            in
            case parseScript title scriptPieces of
                Ok exportLink ->
                    ( { m
                        | editingProgress = DoneEditingScript scriptPieces exportLink
                        , parseError = Nothing
                      }
                    , Cmd.batch [ cmd, scrollToTop ]
                    )

                Err e ->
                    if anyUnsurePieces then
                        ( m, cmd )

                    else
                        ( { m | parseError = Just ("Unable to open script in app: " ++ e) }
                        , Cmd.batch [ cmd, scrollToTop ]
                        )

        Nothing ->
            ( m, cmd )


settingTitleFromSelected : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
settingTitleFromSelected ( m, cmd ) =
    let
        updateTitleFromScriptPiece scriptPieces =
            let
                extractTitle piece =
                    case piece of
                        Just (ScriptPiece TitlePiece line) ->
                            Titled line

                        _ ->
                            m.title

                title =
                    scriptPieces
                        |> List.Extra.getAt m.selectedPiece
                        |> extractTitle
            in
            ( { m | title = title }
            , cmd
            )
    in
    case progressIfEditing m of
        Just scriptPieces ->
            updateTitleFromScriptPiece scriptPieces

        Nothing ->
            ( m, cmd )


progressIfEditing : Model -> Maybe (List ScriptPiece)
progressIfEditing model =
    case model.editingProgress of
        SplittingScript s ->
            Just s

        DoneEditingScript s _ ->
            Just s

        _ ->
            Nothing



--  ____            _       _     _       _             __
-- / ___|  ___ _ __(_)_ __ | |_  (_)_ __ | |_ ___ _ __ / _| __ _  ___ ___
-- \___ \ / __| '__| | '_ \| __| | | '_ \| __/ _ \ '__| |_ / _` |/ __/ _ \
--  ___) | (__| |  | | |_) | |_  | | | | | ||  __/ |  |  _| (_| | (_|  __/
-- |____/ \___|_|  |_| .__/ \__| |_|_| |_|\__\___|_|  |_|  \__,_|\___\___|
--                   |_|
-- Script interface: A user can go through what they've copy-pasted and mark parts
-- FIXME We could work more closely with the elm-pages structure instead of passing views into it


scriptParseApp : Model -> { title : String, body : List (Element Msg) }
scriptParseApp model =
    let
        splitHeader =
            case model.title of
                Titled t ->
                    [ Element.el
                        [ Element.Events.onClick (EditTitle t) ]
                        (Element.text ("Splitting up \"" ++ t ++ "\" into cues"))
                    ]

                EditingTitle old new ->
                    [ Element.row [ Element.spacing 4 ]
                        [ Element.text "Title: "
                        , Element.Input.text [ Element.width (Element.px 400) ]
                            { onChange = EditTitle
                            , text = new
                            , placeholder =
                                Just
                                    (Element.Input.placeholder
                                        []
                                        (Element.text new)
                                    )
                            , label = Element.Input.labelHidden "Title"
                            }
                        , Element.el [ Element.Events.onClick DoneEditingTitle ]
                            (Widget.Icon.elmMaterialIcons Color Material.Icons.save <|
                                { size = 20, color = palette.primary }
                            )
                        , Element.el
                            [ Element.Events.onClick DoneEditingTitle, scaledFont 1 ]
                            (Element.text "Save")
                        ]
                    ]

                Untitled ->
                    [ Element.el
                        [ Element.Events.onClick (EditTitle "") ]
                        (Element.text "Splitting up \"Untitled\" into cues")
                    ]

        loaderHeader =
            [ Element.text "Loading a script from..." ]

        header =
            Element.row
                [ Element.alignLeft
                , scaledFont 3
                , fillWidth
                , Element.paddingXY 50 30
                ]
                (case model.editingProgress of
                    JustStarting ->
                        loaderHeader

                    EditingScript _ _ ->
                        loaderHeader

                    _ ->
                        splitHeader
                )
    in
    { title = "CueCannon - Script Parser"
    , body =
        [ Element.column [ fillWidth ] <|
            [ scriptParseTopBar model
            , toast model
            , header
            , case model.editingProgress of
                JustStarting ->
                    scriptLoaders "" model.loadedScriptPieces

                EditingScript plainScript _ ->
                    scriptLoaders plainScript model.loadedScriptPieces

                SplittingScript scriptPieces ->
                    scriptSplitter model.labelMouseOver model.selectedPiece scriptPieces

                DoneEditingScript scriptPieces _ ->
                    scriptSplitter model.labelMouseOver model.selectedPiece scriptPieces
            ]
        ]
    }


toast : Model -> Element Msg
toast model =
    case model.parseError of
        Nothing ->
            Element.none

        Just err ->
            Element.el
                (fillWidth
                    :: Element.padding 10
                    :: Widget.Material.Color.textAndBackground palette.error
                )
                (Element.text err)


scriptParseTopBar : Model -> Element Msg
scriptParseTopBar { editingProgress } =
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
    buttonWrapper <|
        case editingProgress of
            JustStarting ->
                [ firstButton, arrow, secondButton, arrow, exportButton ]

            EditingScript plainScript oldScriptPieces ->
                let
                    splitScript =
                        makeScriptPieces oldScriptPieces plainScript
                            |> SetScriptPieces
                            |> Just
                in
                [ firstButton
                , arrow
                , { secondButton | onPress = splitScript }
                , arrow
                , exportButton
                ]

            SplittingScript scriptPieces ->
                let
                    plainScript =
                        extractPlainScript scriptPieces
                in
                [ { firstButton
                    | text = "Edit script"
                    , onPress = Just (ChangeScript scriptPieces plainScript)
                  }
                , arrow
                , { secondButton | onPress = Just NoOp }
                , arrow
                , exportButton
                ]

            DoneEditingScript scriptPieces exportLink ->
                let
                    plainScript =
                        extractPlainScript scriptPieces
                in
                [ { firstButton
                    | text = "Edit script"
                    , onPress = Just (ChangeScript scriptPieces plainScript)
                  }
                , arrow
                , { secondButton | onPress = Just NoOp }
                , arrow
                , { icon =
                        Material.Icons.upgrade |> Widget.Icon.elmMaterialIcons Color
                  , text = "Open script in app"
                  , onPress = Just (Export (cueCannonUrl exportLink))
                  }
                ]


scriptLoaders : String -> List ScriptPiece -> Element Msg
scriptLoaders plainScript loadedScriptPieces =
    let
        exampleLoader =
            Element.row [ Element.paddingXY 20 0 ]
                [ Widget.textButton (Material.textButton palette)
                    { onPress =
                        Just (SetScriptPieces (makeScriptPieces [] scene1))
                    , text = "Macbeth"
                    }
                , Widget.textButton (Material.textButton palette)
                    { onPress =
                        Just (SetScriptPieces (makeScriptPieces [] book))
                    , text = "Hamilton"
                    }
                ]

        localStorageLoader =
            Element.el [ Element.paddingXY 20 0 ] <|
                case loadedScriptPieces of
                    [] ->
                        Widget.textButton (Material.textButton palette)
                            { onPress = Nothing, text = "No saved script found" }

                    _ ->
                        Widget.textButton (Material.textButton palette)
                            { onPress = Just ReplaceScriptPiecesWithLoaded
                            , text = "Previously saved script"
                            }

        loaderView label =
            Element.row [ fillWidth, Element.paddingXY 70 30 ]
                [ Element.el [ scaledFont 2, Element.Font.heavy ]
                    (Element.text label)
                ]
    in
    Element.row [ fillWidth ]
        [ Element.column []
            [ loaderView "Copy/Paste"
            , loaderView "Examples"
            , loaderView "Saved"
            , loaderView "AWS"
            ]
        , Element.column
            [ Element.width (Element.fillPortion 3)
            , Element.spacing 40
            ]
            [ copyPasteLoader plainScript
            , exampleLoader
            , localStorageLoader
            , awsLoader
            ]
        ]


copyPasteLoader : String -> Element Msg
copyPasteLoader plainScript =
    Element.el
        [ scaledFont 1
        , Element.alignTop
        , Element.paddingXY 20 0
        , Element.width (Element.maximum 500 Element.fill)
        ]
    <|
        Element.Input.multiline []
            { onChange = ChangeScript []
            , text = plainScript
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Paste here!"))
            , label = Element.Input.labelHidden "Copy/Paste"
            , spellcheck = False
            }


awsLoader : Element Msg
awsLoader =
    Element.el [ Element.paddingXY 20 0 ] <|
        Widget.textButton (Material.textButton palette)
            { onPress = Just RequestAwsFile
            , text = "Upload Transcript"
            }


scriptSplitter : Maybe Int -> Int -> List ScriptPiece -> Element Msg
scriptSplitter labelMouseOver selectedPiece pieces =
    pieces
        |> List.indexedMap (scriptPieceView selectedPiece labelMouseOver)
        |> Element.textColumn
            ([ Element.spacing 5
             , Element.padding 20
             , Element.centerX
             ]
                ++ keyboardShortcutListenerAttributes
            )


scriptPieceView : Int -> Maybe Int -> Int -> ScriptPiece -> Element Msg
scriptPieceView selectedPieceIndex labelIndex index (ScriptPiece kind line) =
    let
        isSelected =
            selectedPieceIndex == index

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
            Element.el
                [ Element.paddingXY 10 0 ]
                (Element.text (labelFromScriptPiece kind))

        mouseOverHelper =
            Element.Events.onMouseEnter (LabelMouseEnter index)
                :: Element.Events.onMouseLeave LabelMouseLeave
                :: (if isHovered then
                        [ Element.onLeft labelHelper ]

                    else
                        []
                   )

        icon =
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
                    |> (\lines -> Element.el [ Element.Font.size 14 ] (Element.text "Mark as: ") :: lines)
                    |> Element.paragraph []
                    |> List.singleton

            else
                []

        viewHelper scriptLine =
            Element.row style
                [ icon
                , Element.textColumn []
                    (Element.paragraph
                        [ Element.Events.onClick (SelectPiece index)
                        ]
                        [ Element.text scriptLine ]
                        :: scriptPieceKindSelector
                    )
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


buttonWrapper buttons =
    Widget.buttonBar (Material.buttonBar [] palette) (barConfig buttons)


barConfig actions =
    { deviceClass = Desktop
    , openLeftSheet = Nothing
    , openRightSheet = Nothing
    , openTopSheet = Nothing
    , primaryActions = actions
    , search = Nothing
    , title =
        Element.row [ Element.spacing 4 ]
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


scrollToTop =
    Task.attempt (\_ -> NoOp) (Browser.Dom.setViewport 0 0)


scrollToScriptPiece scriptPieceIndex =
    case scriptPieceIndex of
        Just index ->
            let
                -- FIXME Just picked from Brave in testing
                scriptPiecePixelHeight =
                    26

                scrollHeight =
                    toFloat (scriptPiecePixelHeight * index)
            in
            Browser.Dom.setViewport 0 scrollHeight
                |> Task.attempt (\_ -> NoOp)

        Nothing ->
            Cmd.none



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
