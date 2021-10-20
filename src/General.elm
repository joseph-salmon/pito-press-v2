module General exposing (Content, Title, getAllTags, getTagSlugs, itemDecoder, itemSingleData, tagDecoder)

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


itemSingleData : String -> DataSource Content
itemSingleData filename =
    File.bodyWithFrontmatter
        (decoder filename)
        ("site/" ++ filename ++ ".md")


decoder : String -> String -> Decoder Content
decoder slug body =
    Decode.map3 (Content body)
        (Decode.succeed slug)
        (Decode.field "title" titleDecoder)
        (Decode.field "descriptiion" Decode.string)
        (Decode.field "publish_date" Decode.string)
        (Decode.field "keywords" (Decode.list Decode.string))
        (Decode.field "published" Decode.string) 
            |> Decode.andThen pubStatusDecoder
    , pageImage : PageImage
        

titleDecoder : Decoder Title
titleDecoder =
    Decode.map Title
        (Decode.field "english" Decode.string)
        (Decode.field "te_reo_maori" Decode.string) 

pubStatusDecoder : String -> Decoder PublishedStatus
pubStatusDecoder status =
        case status of
           "true" ->
                Published

            "false" ->
                Draft




pageImageDecoder : Decoder PageImage
pageImageDecoder =
    {
        
    }