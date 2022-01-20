module Test.Html.SelectorTests exposing (all)

{-| Tests for selectors
-}

import Fuzz exposing (..)
import Html
import Html.Attributes as Attr
import Svg
import Svg.Attributes as SvgAttribs
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


all : Test
all =
    describe "Test.Html.Selector"
        [ bug13
        , textSelectors
        , nsSelectors
        ]


nsSelectors : Test
nsSelectors =
    describe "NS selectors"
        [ test "class selector finds class on svg with one class" <|
            \() ->
                let
                    svgClass =
                        "some-NS-class"
                in
                Svg.svg
                    [ SvgAttribs.class svgClass ]
                    [ Svg.circle [ SvgAttribs.cx "50", SvgAttribs.cy "50", SvgAttribs.r "40" ] [] ]
                    |> Query.fromHtml
                    |> Query.has [ class svgClass ]
        , test "class selector finds class on svg with multiple classes" <|
            \() ->
                let
                    svgClass =
                        "some-NS-class"
                in
                Svg.svg
                    [ SvgAttribs.class svgClass, SvgAttribs.class "another-NS-class" ]
                    [ Svg.circle [ SvgAttribs.cx "50", SvgAttribs.cy "50", SvgAttribs.r "40" ] [] ]
                    |> Query.fromHtml
                    |> Query.has [ class svgClass ]
        , test "classes selector finds all classes on svg" <|
            \() ->
                let
                    svgClass =
                        "some-NS-class"
                in
                Svg.svg
                    [ SvgAttribs.class svgClass, SvgAttribs.class "another-NS-class" ]
                    [ Svg.circle [ SvgAttribs.cx "50", SvgAttribs.cy "50", SvgAttribs.r "40" ] [] ]
                    |> Query.fromHtml
                    |> Query.has [ classes [ svgClass, "another-NS-class" ] ]
        , test "classes selector finds single class on svg with multiple classes" <|
            \() ->
                let
                    svgClass =
                        "some-NS-class"
                in
                Svg.svg
                    [ SvgAttribs.class svgClass, SvgAttribs.class "another-NS-class" ]
                    [ Svg.circle [ SvgAttribs.cx "50", SvgAttribs.cy "50", SvgAttribs.r "40" ] [] ]
                    |> Query.fromHtml
                    |> Query.has [ classes [ svgClass ] ]
        ]


{-| <https://github.com/eeue56/elm-html-test/issues/13>
-}
bug13 : Test
bug13 =
    describe "Reproducing bug #13"
        [ test "Using Selector.text twice checks for both." <|
            \() ->
                Html.div []
                    [ Html.text "Text1"
                    , Html.text "Text2"
                    ]
                    |> Query.fromHtml
                    |> Query.has [ text "Text1", text "Text2" ]
        , test "the welcome <h1> says hello!" <|
            \() ->
                Html.div []
                    [ Html.h1 [ Attr.title "greeting", Attr.class "me" ] [ Html.text "Hello!" ] ]
                    |> Query.fromHtml
                    |> Query.find [ attribute (Attr.title "greeting") ]
                    |> Query.has [ text "Hello!", class "me" ]
        ]


textSelectors : Test
textSelectors =
    describe "Selector.text"
        [ fuzz3 (list string) string (list string) "Finds one result" <|
            \before str after ->
                let
                    textNodes =
                        [ before, [ str ], after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has [ text str ]
        , fuzz3 (list string) (list string) (list string) "Finds multiple results" <|
            \before strings after ->
                let
                    textNodes =
                        [ before, strings, after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has (List.map text strings)
        ]
