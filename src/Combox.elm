module Combox
  exposing
    ( Model
    , Msg (..)
    , Config (..)
    , config
    , view
    , update
    , initial
    , empty
    , title
    , placeholder
    , clear
    , options
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

@docs Model, Msg , Config, config, view, update, initial, empty, title, placeholder, clear, options

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
    , name : String
    , toMsg : Msg -> msg
    , options : List (Html.Attribute Msg)
    , clear : Bool
    }

{-| Create an initial configuration by calling the [`config`](#config) function
  - The [`title`](#title) function defines the title that goes in the header
  - The [`placeholder`](#placeholder) function defines the placeholder of selector
  - The [`name`](#name) function defines the id property of the div
  - The [`clear`](#clear) function defines if the delete selection icon appears
  - The [`options`](#options) function defines the attributes html option list
-}
config : ( Msg -> msg ) -> Config msg
config msg = Config
    { title = Nothing
    , placeholder = Nothing
    , name = "textfield-menu"
    , toMsg = msg
    , clear = True
    , options = []
    }

{-| The custom dropdown menu produces these messages.
-}
type Msg
    = MenuMsg (Selectize.Msg String)
    | SelectText (Maybe String)

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
                Selectize.update SelectText
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

      SelectText newSelection ->
        ({model|selection=newSelection}, Cmd.none)

andDo : Cmd msg -> ( model, Cmd msg ) -> ( model, Cmd msg )
andDo cmd ( model, cmds ) =
  ( model
  , Cmd.batch [ cmd, cmds ]
  )

{-| Change the current settings of clear
--    Combox.clear True
-}
clear : Bool -> Config msg -> Config msg
clear clear (Config config) = Config {config | clear = clear}

{-| Change the current settings of attributes html options list
--    Combox.options [class "combox"]
-}
options : List (Html.Attribute Msg) -> Config msg -> Config msg
options options (Config config) = Config {config | options = options}

{-| Change the current settings of title
--    Combox.title "Languages"
-}
title : String -> Config msg -> Config msg
title title ( Config config ) = Config { config | title = Just title }

{-| Change the current settings of placeholder
--    Combox.placeholder "All"
-}
placeholder : String -> Config msg -> Config msg
placeholder placeholder ( Config config ) = Config { config | placeholder = Just placeholder }

{-| Change the current settings of name
--    Combox.name "id-combox"
-}
name : String -> Config msg -> Config msg
name name ( Config config ) = Config { config | name = name }

{-| The custom dropdown's view function.
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
                        [ ( "selectize__textfield--selection c-black", sthSelected )
                        , ( "selectize__textfield--no-selection c-gray", not sthSelected )
                        , ( "selectize__textfield--menu-open", open )
                        ]
                    , Attributes.autocomplete False
                    ]
            , toggleButton = toggleButton
            , clearButton = if config.clear then clearButton else Nothing
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
                        Html.text "arrow_drop_up"
                      else
                        Html.text "arrow_drop_down"
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
