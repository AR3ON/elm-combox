module Combox  exposing
    ( Model
    , Msg (..)
    , config
    , view
    , update
    , initial
    , empty
    , title
    , placeholder
    , clear
    , options
    , disabled
    )

{-| This is a custom dropdown based on elm-selectize.

    type alias Model =
        { language : Combox.Model }

    init : (Model, Cmd Msg)
    init =
        ( { language = Accordion.empty }, Cmd.none )

    type Msg
        = ComboxMsg String Combox.Msg

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            ComboxMsg selectizeMsg ->
                let
                  ( language, cmd ) =
                      Combox.update msg model.combox
                in
                  ({ model | language = language }, Cmd.map ComboxMsg cmd)

    view : Model -> Html Msg
    view model =
        Html.div []
        [ Html.h3 [] [ Html.text "Dropdown Menus" ]
        , Combox.config ComboxMsg
          |> Combox.clear False
          |> Combox.view model.language
        ]

@docs Model, Msg , config, view, update, initial, empty, title, placeholder, clear, options, disabled

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import Selectize

{-| Define the dropdown identifier and basic elm-selectize model
-}
type alias Model =
  { name : String
  , selection : Maybe String
  , menu : Selectize.State String
  }

{-| Initialize model with data
-}
initial: String -> Maybe String -> List String -> Model
initial name selection items =
  { name = name
  , selection = selection
  , menu = tomenu name items
  }

{-| Initialize empty model
-}
empty: Model
empty =
  { name = ""
  , selection = Nothing
  , menu = tomenu "" []
  }

tomenu : String -> List String -> Selectize.State String
tomenu id items =
  List.map Selectize.entry items
  |> Selectize.closed id identity

{-| Create the view configuration.
-}
type Config msg
  = Config
    { title : Maybe String
    , placeholder : Maybe String
    , toMsg : Msg -> msg
    , options : List (Html.Attribute Msg)
    , clear : Bool
    , disabled : Bool
    }

{-| Create an initial configuration by calling the [`config`](#config) function
  - The [`title`](#title) function defines the title that goes in the header
  - The [`placeholder`](#placeholder) function defines the placeholder of selector
  - The [`clear`](#clear) function defines if the delete selection icon appears
  - The [`options`](#options) function defines the attributes html option list
  - The [`disabled`](#disabled) function defines if the selector is disabled or not
-}
config : ( Msg -> msg ) -> Config msg
config msg = Config
    { title = Nothing
    , placeholder = Nothing
    , toMsg = msg
    , clear = True
    , options = []
    , disabled = False
    }

{-| The custom dropdown menu produces these messages.
-}
type Msg
    = MenuMsg (Selectize.Msg String)
    | Select (Maybe String)

{-| Update the status of the drop-down menu.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      MenuMsg selectizeMsg ->
          let
            newModel =
              {model|menu = newMenu}

            cmd =
              menuCmd
              |> Cmd.map MenuMsg

            ( newMenu, menuCmd, maybeMsg ) =
                Selectize.update Select
                    model.selection
                    model.menu
                    selectizeMsg
          in
            case maybeMsg of
              Nothing ->
                ( newModel, cmd )

              Just nextMsg ->
                update nextMsg newModel
                |> andDo cmd

      Select newSelection ->
        ({model|selection=newSelection}, Cmd.none)

andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
  ( model
  , Cmd.batch [ cmd, cmds ]
  )

{-| change the current settings of clear
-}
clear : Bool -> Config msg -> Config msg
clear clear (Config config) =
  Config {config | clear = clear}

{-| Change the current settings of the options
-}
options : List (Html.Attribute Msg) -> Config msg -> Config msg
options options (Config config) =
  Config {config | options = options}

{-| change the current settings of disabled
-}
disabled : Bool -> Config msg -> Config msg
disabled disabled (Config config) =
  Config {config | disabled = disabled}

{-| change the current settings of the title
-}
title : String -> Config msg -> Config msg
title title ( Config config ) =
  Config { config | title = Just title }

{-| change the current settings of the placeholder
-}
placeholder : String -> Config msg -> Config msg
placeholder placeholder ( Config config ) =
  Config { config | placeholder = Just placeholder }

{-| Create the view configuration, for example

    view: Model -> Html Msg
    view model =
      Html.div []
      [ Html.h3 [] [ Html.text "Dropdown Menus" ]
      , Combox.config ComboxMsg
        |> Combox.view model.language
      ]

-}
view : Model -> Config msg -> Html msg
view model ( Config config ) =
  Html.div config.options
  [ titlewrapper config.title
  , Selectize.view
    (viewAutocomplete ( Config config ))
    model.selection
    model.menu
    |> Html.map MenuMsg
  ] |> Html.map config.toMsg

titlewrapper : Maybe String -> Html msg
titlewrapper tit =
  case tit of
    Nothing -> Html.text ""
    Just title -> Html.label [] [Html.text title]

viewAutocomplete: Config msg -> Selectize.ViewConfig String
viewAutocomplete ( Config config ) =
  viewConfig <|
    Selectize.autocomplete <|
      { attrs =
          \sthSelected open ->
              [ Attributes.class "selectize__textfield"
              , Attributes.classList
                  [ ( "selectize__textfield--selection c-black", sthSelected && (not config.disabled))
                  , ( "selectize__textfield--no-selection c-gray", not sthSelected || config.disabled )
                  , ( "selectize__textfield--menu-open", open )
                  , ( "selectize__textfield--menu-disabled", config.disabled )
                  ]
              , Attributes.disabled config.disabled
              , Attributes.autocomplete False
              ]
      , toggleButton = toggleButton
      , clearButton =
        case (not config.disabled && config.clear) of
          True -> clearButton
          False -> Nothing
      , placeholder = Maybe.withDefault "All" config.placeholder
      }

viewConfig : Selectize.Input String -> Selectize.ViewConfig String
viewConfig selector =
    Selectize.viewConfig
        { container = []
        , menu =
            [ Attributes.class "selectize__menu" ]
        , ul =
            [ Attributes.class "selectize__list" ]
        , entry =
            \tree mouseFocused keyboardFocused ->
                { attributes =
                    [ Attributes.class "selectize__item"
                    , Attributes.classList
                        [ ( "selectize__item--mouse-selected"
                          , mouseFocused
                          )
                        , ( "selectize__item--key-selected"
                          , keyboardFocused
                          )
                        ]
                    ]
                , children =
                    [ Html.text tree ]
                }
        , divider =
            \title ->
                { attributes =
                    [ Attributes.class "selectize__divider" ]
                , children =
                    [ Html.text title ]
                }
        , input = selector
        }

toggleButton : Maybe (Bool -> Html Never)
toggleButton =
    Just <|
        \open ->
            Html.div
                [ Attributes.class "selectize__menu-toggle"
                , Attributes.classList
                    [ ( "selectize__menu-toggle--menu-open", open ) ]
                ]
                [ Html.i
                    [ Attributes.class "material-icons"
                    , Attributes.class "selectize__icon"
                    ]
                    [ if open then
                        Html.text "expand_less"
                      else
                        Html.text "expand_more"
                    ]
                ]

clearButton: Maybe (Html Never)
clearButton =
    Just <|
      Html.div [ Attributes.class "selectize__menu-toggle" ]
      [ Html.i
        [ Attributes.class "material-icons"
        , Attributes.class "selectize__icon"
        ]
        [ Html.text "clear" ]
      ]
