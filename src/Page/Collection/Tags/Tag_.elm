module Page.Collection.Tags.Tag_ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as A
import Item
import List.Extra exposing (unique)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (..)
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


type alias Item =
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
    Item.itemCollectionData
        |> DataSource.map
            (\items ->
                items
                    |> Item.getAllTags
                    |> Item.getTagSlugs
                    |> List.map (\slug -> { tag = slug })
            )


data : RouteParams -> DataSource Data
data routeParams =
    Item.itemCollectionData
        |> DataSource.map
            (\items ->
                let
                    allTags =
                        Item.getAllTags items

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

                    filteredItems =
                        List.filter (\x -> List.member routeParams.tag <| Item.getTagSlugs x.tags) items
                in
                { title = title
                , items = filteredItems
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
    , items : List Item.Item
    }


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Tag: " ++ static.data.title
    , body =
        [ H.h1 []
            [ H.span [] [ H.text "Tags:" ]
            , H.br [] []
            , H.span [] [ H.text static.data.title ]
            , H.ul []
                (List.map
                    (\item ->
                        H.li []
                            [ Route.link (Route.Collection__Slug_ { slug = item.slug }) [] [ H.text item.title ] ]
                    )
                    static.data.items
                )
            ]
        ]
    }
