module Page.About exposing (Data, Model, Msg, page)

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
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


type alias Data =
    General.Content


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    File.bodyWithFrontmatter
        (General.decoder
            ""
        )
        "site/about.md"


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image =
            { url = Url.external ""
            , alt = ""
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.description
        , locale = Nothing
        , title = static.data.title.english
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = String.concat [ static.data.title.teReo, " / ", static.data.title.english] 
    , body =
        [ H.div [ A.class "mb4"] [
            H.img [ A.src <| Url.toString static.data.pageImage.src, A.alt static.data.pageImage.alt] []
         ]
        , H.div [ A.class "f3" ] [ H.text static.data.description ]
        , H.div [] (MarkdownRenderer.mdToHtml static.data.body)
        ]
    }
