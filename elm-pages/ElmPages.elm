module ElmPages exposing (canonicalSiteUrl, generateFiles, manifest, markdownDocument, view)

import Color
import Data.Author as Author
import Date
import Element exposing (Element)
import Element.Font as Font
import Element.Input
import Feed
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Index
import Json.Decode
import Layout
import Markdown.Parser
import Markdown.Renderer
import Metadata exposing (Metadata)
import MySitemap
import Page.Article
import Pages exposing (images, pages)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath exposing (PagePath)
import Pages.StaticHttp as StaticHttp
import Palette



--  _____ _
-- | ____| |_ __ ___    _ __   __ _  __ _  ___  ___
-- |  _| | | '_ ` _ \  | '_ \ / _` |/ _` |/ _ \/ __|
-- | |___| | | | | | | | |_) | (_| | (_| |  __/\__ \
-- |_____|_|_| |_| |_| | .__/ \__,_|\__, |\___||___/
--                     |_|          |___/
--   __                 _                   _   _
--  / _|_ __ ___  _ __ | |_ _ __ ___   __ _| |_| |_ ___ _ __
-- | |_| '__/ _ \| '_ \| __| '_ ` _ \ / _` | __| __/ _ \ '__|
-- |  _| | | (_) | | | | |_| | | | | | (_| | |_| ||  __/ |
-- |_| |_|  \___/|_| |_|\__|_| |_| |_|\__,_|\__|\__\___|_|
--


type alias ScriptParseApp model msg =
    -- Threaded through to seperate App from Elm Pages
    model -> { title : String, body : List (Element msg) }


type alias Rendered msg =
    Element msg


manifest : Manifest.Config Pages.PathKey
manifest =
    { backgroundColor = Just Color.white
    , categories = [ Pages.Manifest.Category.education ]
    , displayMode = Manifest.Standalone
    , orientation = Manifest.Portrait
    , description = "Script Parser - a cue-script extractor"
    , iarcRatingId = Nothing
    , name = "Script Parser"
    , themeColor = Just Color.white
    , startUrl = pages.index
    , shortName = Just "Script Parser"
    , sourceIcon = images.iconPng
    , icons = []
    }


generateFiles :
    List
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        , body : String
        }
    ->
        StaticHttp.Request
            (List
                (Result
                    String
                    { path : List String
                    , content : String
                    }
                )
            )
generateFiles siteMetadata =
    StaticHttp.succeed
        [ Feed.fileToGenerate { siteTagline = siteTagline, siteUrl = canonicalSiteUrl } siteMetadata |> Ok
        , MySitemap.build { siteUrl = canonicalSiteUrl } siteMetadata |> Ok
        ]


markdownDocument : { extension : String, metadata : Json.Decode.Decoder Metadata, body : String -> Result error (Element msg) }
markdownDocument =
    { extension = "md"
    , metadata = Metadata.decoder
    , body =
        \markdownBody ->
            -- Html.div [] [ Markdown.toHtml [] markdownBody ]
            Markdown.Parser.parse markdownBody
                |> Result.withDefault []
                |> Markdown.Renderer.render Markdown.Renderer.defaultHtmlRenderer
                |> Result.withDefault [ Html.text "" ]
                |> Html.div []
                |> Element.html
                |> List.singleton
                |> Element.paragraph [ Element.width Element.fill ]
                |> Ok
    }


view :
    ScriptParseApp model msg
    -> (model -> Element msg)
    -> List ( PagePath Pages.PathKey, Metadata )
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        }
    ->
        StaticHttp.Request
            { view : model -> Rendered msg -> { title : String, body : Html msg }
            , head : List (Head.Tag Pages.PathKey)
            }
view scriptParseApp scriptParseTopBar siteMetadata page =
    StaticHttp.succeed
        { view =
            \model viewForPage ->
                Layout.view
                    (pageView scriptParseApp model siteMetadata page viewForPage)
                    (scriptParseTopBar model)
                    page
        , head = head page.frontmatter
        }


pageView :
    ScriptParseApp model msg
    -> model
    -> List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> Rendered msg
    -> { title : String, body : List (Element msg) }
pageView app model siteMetadata page viewForPage =
    case page.frontmatter of
        Metadata.App ->
            app model

        Metadata.Page metadata ->
            { title = metadata.title
            , body =
                [ viewForPage
                ]

            --        |> Element.textColumn
            --            [ Element.width Element.fill
            --            ]
            }

        Metadata.Article metadata ->
            Page.Article.view metadata viewForPage

        Metadata.Author author ->
            { title = author.name
            , body =
                [ Palette.blogHeading author.name
                , Author.view [] author
                , Element.paragraph [ Element.centerX, Font.center ] [ viewForPage ]
                ]
            }

        Metadata.BlogIndex ->
            { title = "elm-pages blog"
            , body =
                [ Element.column [ Element.padding 20, Element.centerX ] [ Index.view siteMetadata ]
                ]
            }


commonHeadTags : List (Head.Tag Pages.PathKey)
commonHeadTags =
    [ Head.rssLink "/blog/feed.xml"
    , Head.sitemapLink "/sitemap.xml"
    ]



{- Read more about the metadata specs:

   <https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/abouts-cards>
   <https://htmlhead.dev>
   <https://html.spec.whatwg.org/multipage/semantics.html#standard-metadata-names>
   <https://ogp.me/>
-}


head : Metadata -> List (Head.Tag Pages.PathKey)
head metadata =
    commonHeadTags
        ++ (case metadata of
                Metadata.Page meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages-starter"
                        , image =
                            { url = images.iconPng
                            , alt = "elm-pages logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.website

                Metadata.Article meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages starter"
                        , image =
                            { url = meta.image
                            , alt = meta.description
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.description
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.article
                            { tags = []
                            , section = Nothing
                            , publishedTime = Just (Date.toIsoString meta.published)
                            , modifiedTime = Nothing
                            , expirationTime = Nothing
                            }

                Metadata.Author meta ->
                    let
                        ( firstName, lastName ) =
                            case meta.name |> String.split " " of
                                [ first, last ] ->
                                    ( first, last )

                                [ first, middle, last ] ->
                                    ( first ++ " " ++ middle, last )

                                [] ->
                                    ( "", "" )

                                _ ->
                                    ( meta.name, "" )
                    in
                    Seo.summary
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages-starter"
                        , image =
                            { url = meta.avatar
                            , alt = meta.name ++ "'s elm-pages articles."
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.bio
                        , locale = Nothing
                        , title = meta.name ++ "'s elm-pages articles."
                        }
                        |> Seo.profile
                            { firstName = firstName
                            , lastName = lastName
                            , username = Nothing
                            }

                Metadata.App ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages"
                        , image =
                            { url = images.iconPng
                            , alt = "elm-pages logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "elm-pages blog"
                        }
                        |> Seo.website

                Metadata.BlogIndex ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages"
                        , image =
                            { url = images.iconPng
                            , alt = "elm-pages logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "elm-pages blog"
                        }
                        |> Seo.website
           )


canonicalSiteUrl : String
canonicalSiteUrl =
    "https://elm-pages-starter.netlify.com"


siteTagline : String
siteTagline =
    "Starter blog for elm-pages"
