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
import ScriptExport exposing (ScriptPiece(..), cueCannonUrl, parseScript, scriptPiecesFromPlainScript)
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
    = Change String
    | Export String
    | SelectPiece Int
    | NextError


type alias Model =
    { plainScript : String, scriptPieces : List ScriptPiece, selectedPiece : Maybe Int }


init : ( Model, Cmd Msg )
init =
    ( { plainScript = scene1
      , scriptPieces = scriptPiecesFromPlainScript scene1
      , selectedPiece = Nothing
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

        Export href ->
            ( model, Nav.load href )

        NextError ->
            let
                isError piece =
                    case piece of
                        UnsurePiece _ ->
                            True

                        _ ->
                            False
            in
            case List.Extra.findIndex isError model.scriptPieces of
                Just piece ->
                    ( { model | selectedPiece = Just piece }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectPiece i ->
            if List.length model.scriptPieces > i && i >= 0 then
                ( { model | selectedPiece = Just i }, Cmd.none )

            else
                ( model, Cmd.none )


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
                    ( [ unsureButton, characterButton, lineButton, ignoreButton, stageDirectionButton ]
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
    buttonWrapper leftButtons rightButtons


unsureButton =
    { icon = Material.Icons.dangerous |> Widget.Icon.elmMaterialIcons Color
    , text = "Unsure"
    , onPress = Just (Change "")
    }


characterButton =
    { icon = Material.Icons.face |> Widget.Icon.elmMaterialIcons Color
    , text = "Character"
    , onPress = Just (Change "")
    }


lineButton =
    { icon = Material.Icons.receipt |> Widget.Icon.elmMaterialIcons Color
    , text = "Line"
    , onPress = Just (Change "")
    }


ignoreButton =
    { icon = Material.Icons.border_clear |> Widget.Icon.elmMaterialIcons Color
    , text = "Ignore"
    , onPress = Just (Change "")
    }


stageDirectionButton =
    { icon = Material.Icons.directions |> Widget.Icon.elmMaterialIcons Color
    , text = "Stage Direction"
    , onPress = Just (Change "")
    }


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
            { onChange = Change
            , text = plainScript
            , placeholder = Just (Element.Input.placeholder [] (Element.text "Paste a script here to parse into cues!"))
            , label = Element.Input.labelAbove [] <| Element.text ""
            , spellcheck = False
            }


scriptPiecesView : Model -> Element Msg
scriptPiecesView { scriptPieces, selectedPiece } =
    scriptPieces
        |> List.indexedMap (scriptPieceView selectedPiece)
        |> Element.textColumn [ Element.spacing 5, Element.padding 20 ]


scriptPieceView : Maybe Int -> Int -> ScriptPiece -> Element Msg
scriptPieceView selectedPieceIndex index scriptPiece =
    let
        style =
            [ Element.spacing 10
            , Element.Events.onClick (SelectPiece index)
            ]
                ++ (if selectedPieceIndex == Just index then
                        Widget.Material.Color.textAndBackground
                            (Widget.Material.Color.fromCIELCH { l = 94, c = 50, h = 83 })

                    else
                        []
                   )

        viewHelper scriptLine icon =
            Element.row style
                [ iconWrapper icon
                , Element.text (String.fromInt (index + 1))
                , Element.paragraph [] [ Element.text scriptLine ]
                ]
    in
    case scriptPiece of
        UnsurePiece u ->
            viewHelper u Material.Icons.dangerous

        IgnorePiece i ->
            viewHelper i Material.Icons.dangerous

        CharacterPiece c ->
            viewHelper c Material.Icons.dangerous

        LinePiece l ->
            viewHelper l Material.Icons.dangerous

        StageDirectionPiece s ->
            viewHelper s Material.Icons.dangerous


iconWrapper : Material.Icons.Types.Icon Msg -> Element Msg
iconWrapper icon =
    Widget.Icon.elmMaterialIcons Color icon { size = 20, color = palette.error }
