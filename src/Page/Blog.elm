module Page.Blog exposing (Data, Model, Msg, data, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import General
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as A
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (..)
import Post exposing (..)
import Route
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


type alias Data =
    { content : General.Content
    , posts : List Post.Post
    }


data : DataSource Data
data =
    DataSource.map2
        (\a b ->
            { content = a
            , posts = b
            }
        )
        (File.bodyWithFrontmatter
            (General.decoder
                ""
            )
            "site/products.md"
        )
        Post.postCollectionData


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image =
            { url = static.data.content.pageImage.src
            , alt = static.data.content.pageImage.alt
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.content.description
        , locale = Nothing
        , title = static.data.content.title.english
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = String.concat [ static.data.content.title.teReo, " / ", static.data.content.title.english]
    , body =
        [ H.ul []
            (List.map
                (\post ->
                    H.li []
                        [ Route.link
                            (Route.Blog__Slug_ { slug = post.slug })
                            []
                            [ Post.postPreview post
                            ]
                        ]
                )
                static.data.posts
            )
        ]
    }
