module Shared exposing (Data, Markdown, Model, Msg(..), PageImage, PublishedStatus(..), SharedMsg(..), Title, data, dateDecoder, pageImageDecoder, pubStatusDecoder, template, titleDecoder, toHumanDate)

import Browser.Navigation
import DataSource
import DataSource.File as File
import Html as H exposing (Html)
import Html.Attributes as A
import Iso8601
import MarkdownRenderer
import OptimizedDecoder as Decode exposing (Decoder)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Pages.Url as Url
import Path exposing (Path)
import Phosphor exposing (IconWeight(..))
import Route exposing (Route(..))
import SharedTemplate exposing (SharedTemplate)
import Time exposing (..)
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
    , socialIcons : List Social
    }


type SharedMsg
    = NoOp


type alias NavItem =
    { title : String
    , url : String
    }


type alias Social =
    { url : String
    , network : SocialNetwork
    }


type SocialNetwork
    = Facebook
    | Instagram
    | Twitter



-- Common types


type alias Markdown =
    String


type alias Title =
    { english : String
    , teReo : String
    }


type PublishedStatus
    = Draft
    | Published


type alias PageImage =
    { src : Url.Url
    , alt : String
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



-- UTITLITY
-- Date formatting


toDay : Time.Weekday -> String
toDay weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


toMonth : Time.Month -> String
toMonth month =
    case month of
        Jan ->
            "January"

        Feb ->
            "Febrbuary"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


toHumanDate : Time.Posix -> String
toHumanDate time =
    let
        day =
            toWeekday Time.utc time |> toDay

        date =
            String.fromInt <| Time.toDay Time.utc time

        month =
            Time.toMonth Time.utc time |> toMonth

        year =
            String.fromInt <| Time.toYear Time.utc time
    in
    String.concat [ day, ", ", date, " ", month, " ", year ]



-- DECODERS


siteMetaDecoder : Decoder Data
siteMetaDecoder =
    Decode.map3 Data
        (Decode.field "nav" (Decode.list navItemDecoder))
        (Decode.field "site-name" Decode.string)
        (Decode.field "social_links" (Decode.list socialIconDecoder))


navItemDecoder : Decoder NavItem
navItemDecoder =
    Decode.map2 NavItem
        (Decode.field "title" Decode.string)
        (Decode.field "url" Decode.string)


socialIconDecoder : Decoder Social
socialIconDecoder =
    Decode.map2 Social
        (Decode.field "url" Decode.string)
        (Decode.field "network" Decode.string |> Decode.andThen socialNetworkDecoder)


socialNetworkDecoder : String -> Decoder SocialNetwork
socialNetworkDecoder str =
    case str of
        "Instagram" ->
            Decode.succeed Instagram

        "Facebook" ->
            Decode.succeed Facebook

        "Twitter" ->
            Decode.succeed Twitter

        _ ->
            Decode.fail "no social network found"


titleDecoder : Decoder Title
titleDecoder =
    Decode.map2 Title
        (Decode.field "english" Decode.string)
        (Decode.field "te_reo_maori" Decode.string)


pageImageDecoder : Decoder PageImage
pageImageDecoder =
    Decode.map2 PageImage
        (Decode.map Url.external (Decode.field "image" Decode.string))
        (Decode.field "alt" Decode.string)


pubStatusDecoder : Bool -> Decoder PublishedStatus
pubStatusDecoder status =
    if status == True then
        Decode.succeed Published

    else
        Decode.succeed Draft


dateDecoder : String -> Decoder Time.Posix
dateDecoder datestring =
    case Iso8601.toTime datestring of
        Err _ ->
            Decode.fail "Failure parsing date"

        Ok datetime ->
            Decode.succeed datetime


buildTitle : Route -> View msg -> { title : Maybe String, subtitle : Maybe String }
buildTitle route pageView =
    case route of
        Blog__Tags__Tag_ _ ->
            { title = Just "Blog", subtitle = Just pageView.title }

        Blog__Slug_ _ ->
            { title = Just "Blog", subtitle = Just pageView.title }

        Products__Slug_ _ ->
            { title = Just "Products", subtitle = Just pageView.title }

        About ->
            { title = Just pageView.title, subtitle = Nothing }

        Blog ->
            { title = Just pageView.title, subtitle = Nothing }

        Contact ->
            { title = Just pageView.title, subtitle = Nothing }

        Products ->
            { title = Just pageView.title, subtitle = Nothing }

        Testimonials ->
            { title = Just pageView.title, subtitle = Nothing }

        Index ->
            { title = Just pageView.title, subtitle = Nothing }


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { title : String, body : Html msg }
view sharedData page model toMsg pageView =
    let
        title =
            buildTitle (Maybe.withDefault Index page.route) pageView
    in
    { title = pageView.title
    , body =
        H.div [ A.class "eesti navy bg-near-white" ]
            [ H.nav [ A.class "f4 pa0 pa4-l pa3" ]
                [ H.div [ A.class "mw7-l center" ]
                    [ H.div [ A.class "absolute-l top-0-l left-0-l ph3-l pv4-l pa0" ]
                        [ H.a [ A.class "navy link", A.href "/", A.title "Home" ] [ H.img [ A.src "./pitopress.svg", A.width 120, A.height 120, A.alt "Pito Press" ] [] ] ]
                    , H.ul [ A.class "list dib ma0 pa0 pa3-l" ]
                        (List.map
                            (\item ->
                                H.li []
                                    [ H.a [ A.class "navy link dim", A.href <| "/" ++ item.url ] [ H.text item.title ]
                                    ]
                            )
                            sharedData.navItems
                        )
                    , H.div [ A.class " " ]
                        [ if String.contains "Home" pageView.title  then
                            H.text ""

                          else
                            H.h1 [ A.class "normal lh-solid f1 mb0 pa3-l" ]
                                (case title.subtitle of
                                    Just a ->
                                        [ H.span [ A.class "" ] [ H.text <| Maybe.withDefault "" title.title ++ ":" ]
                                        , H.br [] []
                                        , H.text a
                                        ]

                                    Nothing ->
                                        [ H.text <| Maybe.withDefault "" title.title ]
                                )
                        ]
                    ]
                ]
            , H.main_ [ A.class "mw7-l center pv3 ph3" ]
                [ H.div [] pageView.body
                ]
            , H.footer [ A.class "bg-navy " ]
                [ H.div [ A.class "pa4 pv6-m pv6-l mw7-l mw7-m center" ] (socilIconView sharedData.socialIcons)
                ]
            ]
    }



-- ICONS


socilIconView : List Social -> List (Html msg)
socilIconView sn =
    List.map
        (\a ->
            let
                network =
                    case a.network of
                        Instagram ->
                            insta

                        Facebook ->
                            fb

                        Twitter ->
                            twitter
            in
            H.div [ A.class "dib pr2" ]
                [ H.a [ A.class "white", A.href a.url, A.target "_blank" ] [ network ] ]
        )
        sn


insta : Html msg
insta =
    Phosphor.instagramLogo Bold
        |> Phosphor.withSize 2
        |> Phosphor.toHtml []


fb : Html msg
fb =
    Phosphor.facebookLogo Bold
        |> Phosphor.withSize 2
        |> Phosphor.toHtml []


twitter : Html msg
twitter =
    Phosphor.twitterLogo Bold
        |> Phosphor.withSize 2
        |> Phosphor.toHtml []
