module Simple
    exposing
        ( Model
        , Msg(..)
        , init
        , main
        , update
        )

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Combox

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL --
type alias Model =
    { language : Combox.Model
    }

init : ( Model, Cmd Msg )
init =
    ( { language = Combox.initial "id-lang" Nothing languagelist
      }
    , Cmd.none
    )

languagelist : List String
languagelist =
    [ "haskell"
    , "erlang"
    , "elixir"
    , "elm"
    ]

-- UPDATE --
type Msg
    = ComboxMsg Combox.Msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComboxMsg selectizeMsg ->
            let
              ( language, cmd ) =
                  Combox.update selectizeMsg model.language
            in
              ({ model | language = language }, Cmd.map ComboxMsg cmd)

-- SUBSCRIPTIONS --
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

---- VIEW
view : Model -> Html Msg
view model =
    Html.div []
    [ Html.h3 [] [ Html.text "Dropdown Menus" ]
    , Combox.config ComboxMsg
      |> Combox.clear False
      |> Combox.options [Attributes.class "combox"]
      |> Combox.view model.language
    ]
