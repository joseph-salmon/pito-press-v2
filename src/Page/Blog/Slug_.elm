module Page.Blog.Slug_ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import MarkdownRenderer
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Post
import Route exposing (Route(..), link)
import Shared
import Task exposing (Task)
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    Never


type alias RouteParams =
    { slug : String }


type alias Data =
    Post.Post


page : Page RouteParams Data
page =
    Page.prerender
        { head = head
        , routes = routes
        , data = data
        }
        |> Page.buildNoState { view = view }


routes : DataSource (List RouteParams)
routes =
    Post.postCollectionData
        |> DataSource.map
            (\routeParams ->
                routeParams
                    |> List.map
                        (\route ->
                            { slug = route.slug }
                        )
            )


data : RouteParams -> DataSource Data
data routeParams =
    Post.postSingleData routeParams.slug


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
    { title = static.data.title
    , body =
        [ H.h1 [] [ H.text static.data.title ]

        -- , H.div [] [ H.text <| Post.printPubDate static.data.publishDate ]
        , H.div [] (MarkdownRenderer.mdToHtml static.data.body)
        , H.h2 [] [ H.text "Tags" ]
        , H.ul []
            (List.map
                (\( tagsSlug, tagTitle ) ->
                    H.li []
                        [ link (Route.Blog__Tags__Tag_ { tag = tagsSlug }) [] [ H.text tagTitle ]
                        ]
                )
                static.data.tags
            )
        ]
    }
