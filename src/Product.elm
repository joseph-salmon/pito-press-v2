module Product exposing (Product, productCollectionData, productDecoder, productSingleData)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Html as H exposing (Html)
import Html.Attributes as A
import List.Extra exposing (unique)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Path exposing (..)
import Shared exposing (..)
import Time
import View exposing (View)



-- Products
-- contains all data definitions fot the Product type
-- TODO: convert body to Markdown


type alias Product =
    { body : Markdown
    , slug : String
    , title : String
    , description : String
    , publishDate : Time.Posix
    , productImages : List Shared.PageImage
    }


productCollectionData : DataSource (List Product)
productCollectionData =
    Glob.succeed
        (\filePath slug ->
            { filePath = filePath
            , slug = slug
            }
        )
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "site/products/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource
        |> DataSource.map
            (List.map
                (\product ->
                    File.bodyWithFrontmatter (productDecoder product.slug) product.filePath
                )
            )
        |> DataSource.resolve


productSingleData : String -> DataSource Product
productSingleData slug =
    File.bodyWithFrontmatter
        (productDecoder slug)
        ("site/products/" ++ slug ++ ".md")


productDecoder : String -> Shared.Markdown -> Decoder Product
productDecoder slug body =
    Decode.map5 (Product body)
        (Decode.succeed slug)
        (Decode.field "title" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "publish_date" Decode.string |> Decode.andThen Shared.dateDecoder)
        (Decode.field "product_images" (Decode.list Shared.pageImageDecoder))
