module View where

import Models exposing (..)
import Actions exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import Json.Decode as Json exposing ((:=))
import String exposing (join)

------ View ------
view: Address Action -> Model -> Html
view address model =
  div
    []
    [
      (boardView address model),
      inputView address model
    ]

inputView: Address Action -> Model -> Html
inputView address model =
  div
    [
      style [("width", "100%"), ("position","fixed"), ("bottom", "0")]
    ]
    [
      input
        [
          style [("width", "100%") ],
          (placeholder "type..."),
          (autofocus True),
          (value (Maybe.withDefault "" (Maybe.andThen (itemBeingEdited model.items) (Just << .definition)))),
          on "change" targetValue (Signal.message address << DefinitionAccepted)
        ]
        [
        ]
    ]

boardView: Address Action -> Model -> Html
boardView address model =
  div
    [
      class "board",
      style [
        ("position", "relative"),
        ("width", toString model.bottomRight.x ++ "px"),
        ("height", toString model.bottomRight.y ++ "px")
      ],
      onWithOptions "click" {stopPropagation = True, preventDefault = True} eventPos (Signal.message address << ShowPlaceholder)
    ]
    ((List.map (itemView address) model.items) ++ [placeholderView address model.placeholder])


itemView : Address Action -> Item -> Html
itemView address item =
  div
    [
      class (itemClasses item),
      style [
        ("position", "absolute"),
        ("background-color","whitesmoke"),
        ("top", toPxString item.position.y),
        ("left", toPxString item.position.x)],
      onWithOptions "click" {stopPropagation = True, preventDefault = True} Json.value (\_ -> Signal.message address NoOp),
      onDoubleClick address (BeginEdit item.uid)
    ]
    [
      text item.definition
    ]

itemClasses : Item -> String
itemClasses item =
  [("placeholder", item.uid == 0), ("item", True), ("being-edited", item.isEditing)]
  |> List.filter snd
  |> List.map fst
  |> String.join " "

placeholderView : Address Action -> Maybe Item -> Html
placeholderView address maybe =
  case maybe of
    Just item -> itemView address item
    Nothing -> text "click"

toPxString: a -> String
toPxString pos =
  (toString pos) ++ "px"

itemBeingEdited : List Item -> Maybe Item
itemBeingEdited items =
  List.head (List.filter .isEditing items)

eventPos : Json.Decoder Position
eventPos =
  Json.object2
    Position
    ("clientX" := Json.int)
    ("clientY" := Json.int)