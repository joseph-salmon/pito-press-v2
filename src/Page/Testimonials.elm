module Page.Testimonials exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import General
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as A
import MarkdownRenderer
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Product
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
    , products : List Product.Product
    }


data : DataSource Data
data =
    DataSource.map2
        (\a b ->
            { content = a
            , products = b
            }
        )
        (File.bodyWithFrontmatter
            (General.decoder
                ""
            )
            "site/testimonials.md"
        )
        Product.productCollectionData


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
    { title = static.data.content.title.english
    , body =
        [ H.h1 [] [ H.text static.data.content.title.english ]
        , H.div [] (MarkdownRenderer.mdToHtml static.data.content.body)
        , H.ul [ A.class "list pl0" ]
            (List.map
                (\product ->
                    H.li []
                        [ Route.link
                            (Route.Products__Slug_ { slug = product.slug })
                            []
                            [ Product.productPreview product
                            ]
                        ]
                )
                static.data.products
            )
        ]
    }
