module ComboxTests exposing (..)

import Combox
import Html
import Test exposing (Test, test, describe)
import Expect
import Test.Html.Query as Query
import Test.Html.Selector exposing (text, tag, class, classes)

default: Combox.Model
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
        |> Combox.classes "combox"
        |> Combox.view default
    in
      describe "Dropdown"
        [ test "expect wrapping div and class" <|
            \() ->
                html
                |> Query.fromHtml
                |> Query.has [ class "combox", tag "div" ]
        ]
