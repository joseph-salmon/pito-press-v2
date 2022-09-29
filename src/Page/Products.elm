module Page.Products exposing (Data, Model, Msg, page)

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
import Pages.Url as Url
import Product exposing (Product)
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
            "site/products.md"
        )
        Product.productCollectionData


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Pito Press"
        , image =
            { url = static.data.content.pageImage.src
            , alt = static.data.content.pageImage.alt
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.content.description
        , locale = Nothing
        , title = "Products"
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    let  selectWidth =
            if (List.length static.data.products) > 1 then
                "w-50"

            else
                "w-100"

    in
    { title = String.concat [ static.data.content.title.teReo, " / ", static.data.content.title.english]
    , body =
        [ H.div [] (MarkdownRenderer.mdToHtml static.data.content.body)
        , H.ul [ A.class "list pl0 cf w-100" ]
            (List.map
                (\product ->
                    H.li [ A.class <| String.concat [selectWidth, " ", "pa2 fl"] ]
                        [ Route.link
                            (Route.Products__Slug_ { slug = product.slug })
                            []
                            (productPreview product )
                            
                        ]
                )
                static.data.products
            )
        ]
    }

productPreview : Product -> List (Html msg)
productPreview product =
    let
        featuredImage =
            product.productImages
                |> List.head
                |> Maybe.map (\image -> H.img [ A.src <| Url.toString image.src, A.alt image.alt] [])
                |> Maybe.withDefault (H.text "")

        
    in
        [ H.div [ A.class "dim"]
            [ featuredImage
            , H.h2 [ A.class "link navy normal f3 ma0" ] [ H.text product.title ] ]
        
        ]
