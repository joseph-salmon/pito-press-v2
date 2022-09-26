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
import Route
import Shared
import Testimonial
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
    , testimonials : List Testimonial.Testimonial
    }


data : DataSource Data
data =
    DataSource.map2
        (\a b ->
            { content = a
            , testimonials = b
            }
        )
        (File.bodyWithFrontmatter
            (General.decoder
                ""
            )
            "site/testimonials.md"
        )
        Testimonial.testimonialCollectionData


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
        [ H.div [] (MarkdownRenderer.mdToHtml static.data.content.body)
        , H.ul [ A.class "list pl0" ]
            (List.map
                (\testimonial ->
                    H.li []
                        (H.h2 [] [ H.text testimonial.name ]
                            :: MarkdownRenderer.mdToHtml testimonial.body
                        )
                )
                static.data.testimonials
            )
        ]
    }
