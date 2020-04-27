module ComboxTests exposing (..)

import Combox
import Expect
import Html
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, classes, tag, text)


default : Combox.Model
default =
    Combox.initial "id-lang" Nothing languagelist


languagelist : List String
languagelist =
    [ "haskell"
    , "erlang"
    , "elixir"
    , "elm"
    ]


all : Test
all =
    let
        html =
            Combox.config (\_ -> ())
                |> Combox.view default
    in
    describe "Dropdown"
        [ test "expect wrapping div and class" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.has [ tag "div" ]
        ]
