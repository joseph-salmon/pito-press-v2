module Testimonial exposing (Testimonial, testimonialCollectionData, testimonialDecoder)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Html as H exposing (Html)
import Html.Attributes as A
import List.Extra exposing (unique)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url as Url
import Path exposing (..)
import Route
import Shared exposing (..)
import Time
import View exposing (View)



-- Products
-- contains all data definitions fot the Testimonial type
-- TODO: convert body to Markdown


type alias Testimonial =
    { body : Markdown
    , name : String
    }


testimonialCollectionData : DataSource (List Testimonial)
testimonialCollectionData =
    Glob.succeed
        (\filePath slug ->
            { filePath = filePath
            , slug = slug
            }
        )
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "site/testimonials/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource
        |> DataSource.map
            (List.map
                (\testimonial ->
                    File.bodyWithFrontmatter testimonialDecoder testimonial.filePath
                )
            )
        |> DataSource.resolve


testimonialDecoder : Shared.Markdown -> Decoder Testimonial
testimonialDecoder body =
    Decode.map (Testimonial body)
        (Decode.field "name" Decode.string)
