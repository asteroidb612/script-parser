module Main exposing (main)

import Browser.Navigation as Nav
import Color
import Element exposing (DeviceClass(..), Element)
import Element.Font
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
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

        Export href ->
            ( model, Nav.load href )


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
            [ bar model
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


scriptEditor : Model -> Element Msg
scriptEditor { plainScript } =
    let
        fontSize =
            if plainScript /= "" then
                12

            else
                18
    in
    Element.el
        [ Element.width (Element.px 350)
        , Element.alignRight
        , Element.alignTop
        , Element.Font.size fontSize
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
scriptPiecesView { scriptPieces } =
    scriptPieces
        |> List.map scriptPieceView
        |> Element.textColumn [ Element.spacing 5, Element.padding 20 ]


scriptPieceView : ScriptPiece -> Element Msg
scriptPieceView scriptPiece =
    case scriptPiece of
        UnsurePiece u ->
            Element.row [ Element.spacing 10 ]
                [ iconWrapper Material.Icons.dangerous
                , Element.paragraph [] [ Element.text u ]
                ]

        _ ->
            Element.none


iconWrapper : Material.Icons.Types.Icon Msg -> Element Msg
iconWrapper icon =
    Widget.Icon.elmMaterialIcons Color icon { size = 20, color = Color.blue }


menuStyle =
    let
        p =
            Material.menuBar palette

        x =
            p.content.menu
                |> Debug.log "menuBar"
    in
    p


bar : Model -> Element Msg
bar model =
    Widget.menuBar menuStyle
        { title =
            "Script Parser"
                |> Element.text
        , deviceClass = Desktop
        , openRightSheet = Nothing
        , openLeftSheet = Nothing
        , openTopSheet = Nothing
        , primaryActions =
            case parseScript model.scriptPieces of
                Err s ->
                    [ { icon =
                            Material.Icons.bug_report
                                |> Widget.Icon.elmMaterialIcons Color
                      , text = "Error parsing: " ++ s
                      , onPress = Just (Change "")
                      }
                    ]

                Ok href ->
                    [ { icon =
                            Material.Icons.upgrade
                                |> Widget.Icon.elmMaterialIcons Color
                      , text = "Open script in app"
                      , onPress = Just (Export (cueCannonUrl href))
                      }
                    ]
        , search = Nothing
        }
