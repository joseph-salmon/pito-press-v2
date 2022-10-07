module MarkdownRenderer exposing (htmlRenderer, mdToHtml)

import Html as H exposing (Html)
import Html.Attributes as A
import Markdown.Block as Block
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer


htmlRenderer : Markdown.Renderer.Renderer (Html msg)
htmlRenderer =
    { heading =
        \{ level, children } ->
            case level of
                Block.H1 ->
                    H.h1 [ A.class "f1"] children

                Block.H2 ->
                    H.h2 [ A.class "f2"] children

                Block.H3 ->
                    H.h3 [ A.class "f3" ] children

                Block.H4 ->
                    H.h4 [ A.class "f4" ] children

                Block.H5 ->
                    H.h5 [ A.class "f5"] children

                Block.H6 ->
                    H.h6 [A.class "f6" ] children
    , paragraph = H.p [ A.class "lh-title f4" ]
    , hardLineBreak = H.br [ A.class "lh-title f4" ] []
    , blockQuote = H.blockquote [ A.class "lh-title f4" ]
    , strong =
        \children -> H.strong [] children
    , emphasis =
        \children -> H.em [] children
    , strikethrough =
        \children -> H.del [] children
    , codeSpan =
        \content -> H.code [] [ H.text content ]
    , link =
        \link content ->
            case link.title of
                Just title ->
                    H.a
                        [ A.href link.destination
                        , A.title title
                        , A.class "bg-gold navy"
                        ]
                        content

                Nothing ->
                    H.a [ A.href link.destination, A.class "underline red dim" ] content
    , image =
        \imageInfo ->
            case imageInfo.title of
                Just title ->
                    H.img
                        [ A.src imageInfo.src
                        , A.alt imageInfo.alt
                        , A.title title
                        ]
                        []

                Nothing ->
                    H.img
                        [ A.src imageInfo.src
                        , A.alt imageInfo.alt
                        ]
                        []
    , text =
        H.text
    , unorderedList =
        \items ->
            H.ul [ A.class "pl3 lh-title f4"]
                (items
                    |> List.map
                        (\item ->
                            case item of
                                Block.ListItem task children ->
                                    let
                                        checkbox =
                                            case task of
                                                Block.NoTask ->
                                                    H.text ""

                                                Block.IncompleteTask ->
                                                    H.input
                                                        [ A.disabled True
                                                        , A.checked False
                                                        , A.type_ "checkbox"
                                                        ]
                                                        []

                                                Block.CompletedTask ->
                                                    H.input
                                                        [ A.disabled True
                                                        , A.checked True
                                                        , A.type_ "checkbox"
                                                        ]
                                                        []
                                    in
                                    H.li [ ] (checkbox :: children)
                        )
                )
    , orderedList =
        \startingIndex items ->
            H.ol
                (case startingIndex of
                    1 ->
                        [ A.start startingIndex, A.class "lh-title f4" ]

                    _ ->
                        [ A.class "lh-title f4" ]
                )
                (items
                    |> List.map
                        (\itemBlocks ->
                            H.li []
                                itemBlocks
                        )
                )
    , html = Markdown.Html.oneOf []
    , codeBlock =
        \{ body, language } ->
            let
                classes =
                    -- Only the first word is used in the class
                    case Maybe.map String.words language of
                        Just (actualLanguage :: _) ->
                            [ A.class <| "language-" ++ actualLanguage ]

                        _ ->
                            []
            in
            H.pre []
                [ H.code classes
                    [ H.text body
                    ]
                ]
    , thematicBreak = H.hr [] []
    , table = H.table [ A.class "f6 w-100 mw8 center" ]
    , tableHeader = H.thead []
    , tableBody = H.tbody [ A.class "lh-copy" ]
    , tableRow = H.tr []
    , tableHeaderCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map A.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            H.th (A.class "fw6 bb b--black-20 tl pb3 pr3 bg-white" :: attrs)
    , tableCell =
        \maybeAlignment ->
            let
                attrs =
                    maybeAlignment
                        |> Maybe.map
                            (\alignment ->
                                case alignment of
                                    Block.AlignLeft ->
                                        "left"

                                    Block.AlignCenter ->
                                        "center"

                                    Block.AlignRight ->
                                        "right"
                            )
                        |> Maybe.map A.align
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []
            in
            H.td (A.class "pv3 pr3 bb b--black-20" :: attrs)
    }


mdToHtml : String -> List (H.Html msg)
mdToHtml markdownInput =
    case
        markdownInput
            |> Markdown.parse
            |> Result.mapError deadEndsToString
            |> Result.andThen (\ast -> Markdown.Renderer.render htmlRenderer ast)
    of
        Ok rendered ->
            rendered

        Err errors ->
            [ H.text errors ]


deadEndsToString deadEnds =
    deadEnds
        |> List.map Markdown.deadEndToString
        |> String.join "\n"
