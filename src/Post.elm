module Post exposing (Post, Tag, getAllTags, getTagSlugs, postCollectionData, postDecoder, postSingleData, tagDecoder)

import DataSource exposing (DataSource)
import DataSource.File as File exposing (onlyFrontmatter)
import DataSource.Glob as Glob
import Iso8601
import List.Extra exposing (unique)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (..)
import Route
import Shared
import Time
import View exposing (View)



-- Posts
-- contains all data definitions fot the Post type
-- TODO: convert body to Markdown


type alias Post =
    { body : Markdown
    , slug : String
    , title : String
    , intro : String
    , published : Shared.PublishedStatus
    , publishDate : Time.Posix
    , pageImage : Shared.PageImage
    , tags : List Tag
    }


type alias Markdown =
    String



-- title: Test post
-- intro: Some introductory text
-- published: true
-- publish_date: 2021-10-18T11:00:00Z
-- page_image: "/uploads/woodcut.jpg"
-- tags:


type alias Tag =
    ( String, String )


postCollectionData : DataSource (List Post)
postCollectionData =
    Glob.succeed
        (\filePath slug ->
            { filePath = filePath
            , slug = slug
            }
        )
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal "site/blog/")
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".md")
        |> Glob.toDataSource
        |> DataSource.map
            (List.map
                (\post ->
                    File.bodyWithFrontmatter (postDecoder post.slug) post.filePath
                )
            )
        |> DataSource.resolve


postSingleData : String -> DataSource Post
postSingleData slug =
    File.bodyWithFrontmatter
        (postDecoder slug)
        ("site/blog/" ++ slug ++ ".md")


getAllTags : List Post -> List Tag
getAllTags posts =
    posts
        |> List.concatMap (\post -> post.tags)
        |> unique



-- Get the tag slugs


getTagSlugs : List Tag -> List String
getTagSlugs =
    List.map Tuple.first



-- print the publish date
-- printPubDate : Time.Posix -> String
-- printPubDate dateTime =
--     let
--         hour =
--             String.fromInt (Time.toHour Time.utc dateTime)
--         minute =
--             String.fromInt (Time.toMinute Time.utc dateTime)
--         second =
--             String.fromInt (Time.toSecond Time.utc dateTime)
--     in
--     hour ++ ":" ++ minute ++ ":" ++ second


postDecoder : Shared.Markdown -> String -> Decoder Post
postDecoder slug body =
    Decode.map7 (Post body)
        (Decode.succeed slug)
        (Decode.field "title" Decode.string)
        (Decode.field "intro" Decode.string)
        (Decode.field "published" Decode.bool |> Decode.andThen Shared.pubStatusDecoder)
        (Decode.field "publish_date" Decode.string |> Decode.andThen dateDecoder)
        (Decode.field "page_image" Shared.pageImageDecoder)
        (Decode.field "tags" <|
            Decode.list (Decode.andThen tagDecoder Decode.string)
        )


dateDecoder : String -> Decoder Time.Posix
dateDecoder datestring =
    case Iso8601.toTime datestring of
        Err _ ->
            Decode.fail "Failure parsing date"

        Ok datetime ->
            Decode.succeed datetime



-- Gather all the tags from all Posts, flatten the list and remove duplicates


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
