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
        , siteName = static.sharedData.siteName
        , image =
            { url = Pages.Url.external ""
            , alt = ""
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = static.data.content.description
        , locale = Nothing
        , title = static.data.content.title.english
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = String.concat [ static.data.content.title.teReo, " / ", static.data.content.title.english]
    , body =
        [ H.div [ A.class "f3" ] (MarkdownRenderer.mdToHtml static.data.content.description)
        , H.div [] (MarkdownRenderer.mdToHtml static.data.content.body)
        , if List.isEmpty static.data.testimonials then
            H.text ""

          else
            H.div []
                [ H.h2 []
                    [ H.text "Testimonials"
                    ]
                , H.ul [ A.class "list pl0 cf" ]
                    (List.map
                        (\testimonial ->
                            H.li [ A.class "fl w-50-l pa1" ]
                                [ H.blockquote [ A.class "ma0 ph3 pv1 bg-white shadow-4 br2" ]
                                    (List.append (MarkdownRenderer.mdToHtml testimonial.body) [ H.div [ A.class "pb3 tr ttu f6" ] [ H.text ("— " ++ testimonial.name) ] ])
                                ]
                        )
                        static.data.testimonials
                    )
                ]
        ]
    }
