module General exposing (Content, Title, decoder, titleDecoder)

import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import List.Extra exposing (unique)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (..)
import Route
import Shared
import View exposing (View)



-- General page type
-- contains all data definitions for the General page type
-- TODO: convert body to Markdown


type alias Content =
    { body : String
    , slug : String
    , title : Title
    , description : String
    , publishDate : String
    , keywords : List String
    , published : PublishedStatus
    , pageImage : PageImage
    }


type alias Title =
    { english : String
    , teReo : String
    }


type PublishedStatus
    = Draft
    | Published


type alias PageImage =
    { src : String
    , alt : String
    }


decoder : String -> String -> Decoder Content
decoder slug body =
    Decode.map7 (Content body)
        (Decode.succeed slug)
        (Decode.field "title" titleDecoder)
        (Decode.field "description" Decode.string)
        (Decode.field "publish_date" Decode.string)
        (Decode.field "keywords" (Decode.list Decode.string))
        (Decode.field "published" Decode.bool
            |> Decode.andThen pubStatusDecoder
        )
        (Decode.field "page_image" pageImageDecoder)


titleDecoder : Decoder Title
titleDecoder =
    Decode.map2 Title
        (Decode.field "english" Decode.string)
        (Decode.field "te_reo_maori" Decode.string)


pubStatusDecoder : Bool -> Decoder PublishedStatus
pubStatusDecoder status =
    if status == True then
        Decode.succeed Published

    else
        Decode.succeed Draft


pageImageDecoder : Decoder PageImage
pageImageDecoder =
    Decode.map2 PageImage
        (Decode.field "image" Decode.string)
        (Decode.field "alt_text" Decode.string)
