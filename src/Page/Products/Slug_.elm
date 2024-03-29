module Page.Products.Slug_ exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import Html.Attributes as A
import MarkdownRenderer
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url as Url
import Product
import Route exposing (Route(..), link)
import Shared
import Time
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    { slug : String }


type alias Data =
    Product.Product


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
    Product.productCollectionData
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
    Product.productSingleData routeParams.slug


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    let
        pageImage =
            case static.data.productImages of
                [] ->
                    { src = Url.external "", alt = "" }

                [ x ] ->
                    x

                x :: xs ->
                    x
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "Pito Press"
        , image =
            { url = pageImage.src
            , alt = pageImage.alt
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.description
        , locale = Nothing
        , title = static.data.title
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
        [ -- TODO: Format date
          -- Day, 13 July 1981
          H.div [ A.class "mb4"]
            (static.data.productImages
                |> List.map (\image -> H.img [ A.src <| Url.toString image.src, A.alt image.alt, A.class "mb2"] [])
            )
        , H.div [ A.class "f3" ] [ H.text static.data.description ]
        , H.div [] <| MarkdownRenderer.mdToHtml static.data.body
        ]
    }
