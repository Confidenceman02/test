module Test.Html.SelectorTests exposing (all)

{-| Tests for selectors
-}

import Fuzz exposing (..)
import Html
import Html.Attributes as Attr
import Json.Encode
import Svg
import Svg.Attributes as SvgAttribs
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (..)


all : Test
all =
    describe "Test.Html.Selector"
        [ bug13
        , textSelectors
        , classSelectors
        ]


classSelectors : Test
classSelectors =
    describe "class attribute assertions"
        [ describe "Using Html.Attribute.class" <|
            classAssertions [ "some-custom-class" ] <|
                Html.div [ Attr.class "some-custom-class" ] []
        , describe "Using Html.Attribute.property" <|
            classAssertions [ "some-custom-class" ] <|
                Html.div [ Attr.property "className" (Json.Encode.string "some-custom-class") ] []
        , describe "Using Html.Attribute.attribute" <|
            classAssertions [ "some-custom-class" ] <|
                Html.div [ Attr.attribute "class" "some-custom-class" ] []
        , describe "svg with one class" <|
            classAssertions [ "some-NS-class" ] <|
                Svg.svg [ SvgAttribs.class "some-NS-class" ] []
        , describe "with multiple classes" <|
            classAssertions [ "some-NS-class", "another-NS-class" ] <|
                Svg.svg
                    [ SvgAttribs.class "some-NS-class"
                    , SvgAttribs.class "another-NS-class"
                    ]
                    []
        ]


classAssertions : List String -> Html.Html msg -> List Test
classAssertions classes html =
    (test "Test.Html.Selector.classes finds the classes" <|
        \_ ->
            html
                |> Query.fromHtml
                |> Query.has [ Selector.classes classes ]
    )
        :: List.map
            (\className ->
                test ("Test.Html.Selector.class finds the class: " ++ className) <|
                    \_ ->
                        html
                            |> Query.fromHtml
                            |> Query.has [ Selector.class className ]
            )
            classes


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
                    |> Query.has [ Selector.text "Hello!", Selector.class "me" ]
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
