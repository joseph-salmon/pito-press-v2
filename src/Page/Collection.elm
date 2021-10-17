module Page.Collection exposing (Data, Model, Msg, data, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as Attr
import Item
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
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


type alias Data =
    { title : String
    , items : List Item.Item
    }


data : DataSource Data
data =
    DataSource.map2
        (\a b ->
            { title = a
            , items = b
            }
        )
        pageDecoder
        Item.itemCollectionData


pageDecoder : DataSource String
pageDecoder =
    File.onlyFrontmatter (Decode.field "title" Decode.string) "site/index.md"


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
    { title = "Collection"
    , body =
        [ H.ul []
            (List.map
                (\item ->
                    H.li []
                        [ Route.link 
                            (Route.Collection__Slug_ { slug = item.slug })
                            []
                            [ H.text item.title ]  ]
                        
                )
                static.data.items
            )
        ]
    }
