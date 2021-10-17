module Shared exposing (Data, Model, Msg(..), SharedMsg(..), data, template)

import Browser.Navigation
import DataSource
import DataSource.File as File
import Html exposing (Html)
import Html.Attributes as Attrs
import OptimizedDecoder as Decode exposing (Decoder)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg


type alias Data =
    { navItems : List NavItem
    , siteName : String
    }


type SharedMsg
    = NoOp


type alias NavItem =
    { name : String
    , url : String
    }


type alias Model =
    { showMobileMenu : Bool
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init navigationKey flags maybePagePath =
    ( { showMobileMenu = False }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }, Cmd.none )

        SharedMsg globalMsg ->
            ( model, Cmd.none )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    File.onlyFrontmatter siteMetaDecoder "site/global.md"


siteMetaDecoder : Decoder Data
siteMetaDecoder =
    Decode.map2 Data
        (Decode.field "nav" (Decode.list navItemDecoder))
        (Decode.field "site-name" Decode.string)


navItemDecoder : Decoder NavItem
navItemDecoder =
    Decode.map2 NavItem
        (Decode.field "name" Decode.string)
        (Decode.field "url" Decode.string)


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    { title = pageView.title
    , body =
        Html.div []
            [ Html.ul []
                (List.map
                    (\a ->
                        Html.li []
                            [ Html.a [ Attrs.href <| "/" ++ a.url ] [ Html.text a.name ]
                            ]
                    )
                    sharedData.navItems
                )
            , Html.div [] pageView.body
            ]
    }
