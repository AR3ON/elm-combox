module Complex
    exposing
        ( Model
        , Msg(..)
        , init
        , main
        , update
        )

import Html exposing (Html)
import Html.Attributes as Attributes
import Combox
import Dict exposing (Dict)

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- MODEL --
type alias Model =
    { combox : Dict String Combox.Model
    }

init : ( Model, Cmd Msg )
init =
    ( { combox = initialcombox
      }
    , Cmd.none
    )

initialcombox: Dict String Combox.Model
initialcombox =
  let
    make (item) =
      ( toString item.name
      , Combox.initial (toString item.name) Nothing item.items
      )
  in
  [ { name= Frontend, items = frontends}
  , { name= Backend, items= backends}
  ] |> List.map make
    |> Dict.fromList

-- UPDATE --
type Msg
    = ComboxMsg Language Combox.Msg

type Language
    = Frontend
    | Backend

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ComboxMsg lang selectizeMsg ->
            let
              ( combox, cmd ) =
                  Combox.update selectizeMsg (getcombox model.combox lang)
            in
              ({ model | combox = Dict.insert (toString lang) combox model.combox}
              , Cmd.map (ComboxMsg lang) cmd
              )

getcombox: Dict String Combox.Model -> Language -> Combox.Model
getcombox dict selectize =
  Dict.get (toString selectize) dict
  |> Maybe.withDefault Combox.empty

-- SUBSCRIPTIONS --
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

---- VIEW

view : Model -> Html Msg
view model =
    Html.div []
    [ new model Frontend "Dropdown Menus - Frontend"
    , new model Backend "Dropdown Menus - Backend"
    ]

new: Model -> Language -> String -> Html Msg
new model lang title =
  Html.div []
  [ Html.h3 [] [ Html.text title ]
  , Combox.config (ComboxMsg lang)
    |> Combox.view (getcombox model.combox lang)
  ]
-- DATA --

backends : List String
backends =
    [ "erlang"
    , "elixir"
    ]

frontends : List String
frontends =
    [ "elm"
    ]
