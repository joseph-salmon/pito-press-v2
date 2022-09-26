module Page.Blog.Tags.Tag_ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import General
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as A
import List.Extra exposing (unique)
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
    { tag : String }



-- We ue a tuple so that tags are comparable


type alias Tag =
    ( String, String )


type alias Post =
    { slug : String
    , title : String
    , tags : List Tag
    }


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }



-- TODO: Collect all tag instances and remove duplicates


routes : DataSource (List RouteParams)
routes =
    Post.postCollectionData
        |> DataSource.map
            (\posts ->
                posts
                    |> Post.getAllTags
                    |> Post.getTagSlugs
                    |> List.map (\slug -> { tag = slug })
            )


data : RouteParams -> DataSource Data
data routeParams =
    Post.postCollectionData
        |> DataSource.map
            (\posts ->
                let
                    allTags =
                        Post.getAllTags posts

                    title =
                        allTags
                            |> List.filter (\( x, y ) -> x == routeParams.tag)
                            |> (\tags ->
                                    case tags of
                                        [] ->
                                            "No tags!"

                                        x :: xs ->
                                            Tuple.second x
                               )

                    filteredPosts =
                        List.filter (\x -> List.member routeParams.tag <| Post.getTagSlugs x.tags) posts
                in
                { title = title
                , posts = filteredPosts
                }
            )


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


type alias Data =
    { title : String
    , posts : List Post.Post
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Tag: " ++ static.data.title
    , body =
        [ H.ul []
            (List.map
                (\post ->
                    H.li []
                        [ Route.link (Route.Blog__Slug_ { slug = post.slug })
                            []
                            [ H.h2 [] [ H.text post.title ]
                            , H.text post.intro
                            ]
                        ]
                )
                static.data.posts
            )
        ]
    }
