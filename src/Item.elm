module Item exposing (..)

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



-- Items

-- contains all data definitions fot the Item type

-- TODO: convert body to Markdown
type alias Item =
    { body : String
    , slug : String
    , title : String
    , tags : List Tag
    }

type alias Tag =
    ( String, String )



itemCollectionData : DataSource (List Item)
itemCollectionData =
    Glob.succeed
        (\filePath slug ->
            { filePath = filePath
            , slug = slug
            }
        )
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "site/collection/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource
        |> DataSource.map
            (List.map
                (\item ->
                    File.bodyWithFrontmatter (itemDecoder item.slug) item.filePath
                )
            )
        |> DataSource.resolve



itemSingleData : String -> DataSource Item
itemSingleData slug =
    File.bodyWithFrontmatter
        (itemDecoder slug)
        ("site/collection/" ++ slug ++ ".md")


getAllTags : List Item -> List Tag
getAllTags items =
    items
        |> List.concatMap (\item -> item.tags)
        |> unique



-- Get the tag slugs


getTagSlugs : List Tag -> List String
getTagSlugs =
    List.map Tuple.first


itemDecoder : String -> String -> Decoder Item
itemDecoder  slug body =
    Decode.map3 (Item body)
        (Decode.succeed slug)
        (Decode.field "title" Decode.string)
        (Decode.field "tags" <|
            Decode.list (Decode.andThen tagDecoder Decode.string)
        )

-- Gather all the tags from all items, flatten the list and remove duplicates


tagDecoder : String -> Decoder Tag
tagDecoder tag =
    let
        slugFormat =
            tag
                |> String.trim
                |> String.replace " " "-"
                |> String.toLower
    in
    Decode.map2 Tuple.pair
        (Decode.succeed <| slugFormat)
        (Decode.succeed tag)
