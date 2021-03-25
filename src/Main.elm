module Main exposing (main)

import Base64
import Element exposing (Element)
import Element.Input
import ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)
import Json.Encode
import Metadata exposing (Metadata)
import Pages exposing (images, pages)
import Pages.Platform



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


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius.This is the dawning of the Age of Aquarius. This is the dawning of the Age of Aquarius. ", Cmd.none )


type Msg
    = Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change s ->
            ( s, Cmd.none )


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
    { title = "Cue Extractor"
    , body =
        [ Element.column [ Element.width Element.fill ]
            [ Element.row []
                [ Element.el [ Element.width Element.fill ] <| scriptEditor model
                , Element.el [ Element.width Element.fill, Element.alignTop ] <| parsedScript model
                ]
            ]
        ]
    }


scriptEditor model =
    Element.el [ Element.width Element.fill ] <|
        Element.Input.multiline []
            { onChange = Change
            , text = model
            , placeholder = Nothing
            , label = Element.Input.labelAbove [] <| Element.text ""
            , spellcheck = False
            }


parsedScript model =
    model
        |> String.split "\n"
        |> List.map Element.text
        |> List.map List.singleton
        |> List.map (Element.paragraph [])
        |> Element.textColumn [ Element.spacing 5, Element.padding 40 ]



--  ____            _       _     _____                       _
-- / ___|  ___ _ __(_)_ __ | |_  | ____|_  ___ __   ___  _ __| |_
-- \___ \ / __| '__| | '_ \| __| |  _| \ \/ / '_ \ / _ \| '__| __|
--  ___) | (__| |  | | |_) | |_  | |___ >  <| |_) | (_) | |  | |_
-- |____/ \___|_|  |_| .__/ \__| |_____/_/\_\ .__/ \___/|_|   \__|
--                   |_|                    |_|


type alias Line =
    { speaker : String, identifier : String, line : String }


type alias Script =
    { title : String
    , lines : List Line
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


lineEncoder : String -> Line -> Json.Encode.Value
lineEncoder title { speaker, identifier, line } =
    Json.Encode.object
        [ ( "t", Json.Encode.string line )
        , ( "s", Json.Encode.string speaker )
        , ( "l", Json.Encode.string identifier )
        , ( "p", Json.Encode.string title )
        ]
