module Page.Index exposing (Data, Model, Msg, page)

import DataSource exposing (DataSource)
import DataSource.File as File
import Head
import Head.Seo as Seo
import Html as H exposing (Html)
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


type alias Data =
    { title : String
    , rows : List String
    }


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    File.onlyFrontmatter decoder "site/index.md"


decoder : Decoder Data
decoder =
    Decode.map2 Data
        (Decode.field "title" Decode.string)
        (Decode.field "rows" <| Decode.list Decode.string)


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = static.sharedData.siteName
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = static.sharedData.siteName
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
        [ H.ul []
            (List.map
                (\row ->
                    H.li []
                        [ H.text row ]
                )
                static.data.rows
            )
        ]
    }
