module Page.Blog exposing (Data, Model, Msg, data, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import General
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as Attr
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (..)
import Post
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
    { title : Shared.Title
    , posts : List Post.Post
    }


data : DataSource Data
data =
    DataSource.map2
        (\a b ->
            { title = a
            , posts = b
            }
        )
        (File.onlyFrontmatter (Decode.field "title" General.titleDecoder) "site/index.md")
        Post.postCollectionData


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Blog"
    , body =
        [ H.h1 [] [ H.text static.data.title.english ]
        , H.ul []
            (List.map
                (\post ->
                    H.li []
                        [ Route.link
                            (Route.Blog__Slug_ { slug = post.slug })
                            []
                            [ H.text post.title ]
                        ]
                )
                static.data.posts
            )
        ]
    }
