module App where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)
import Json.Decode as Json exposing ((:=))

------ Models ------

type alias Model = {
  items : List Item,
  nextuid : Int,
  bottomRight : Position,
  placeholder : Maybe Item
}

type alias Item = {
  definition : String,
  position : Position,
  uid : Int
}

type alias Position = {
  x : Int,
  y : Int
}

initialModel: Model
initialModel =
  emptyModel

emptyModel: Model
emptyModel =
  {
    items = [],
    nextuid = 1,
    placeholder = Nothing,
    bottomRight = {x = 1000, y = 1000}
  }

------ Updates ------

type Action
  = NoOp
  | ShowPlaceholder Position
  | UpdateDefinition String
  | Add String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model

    ShowPlaceholder position -> {model | placeholder = Just (Item "placeholder" position 0)}

    UpdateDefinition definition ->
      {model | placeholder = (updatePlaceholderDefinition model.placeholder definition)}

    Add definition ->
      let item = {
        definition = definition,
        position = (getPositionFromPlaceholder model.placeholder),
        uid = model.nextuid
      }
      in
        { model |
          placeholder = Nothing,
          items = model.items ++ [item],
          nextuid = model.nextuid + 1
        }


getPositionFromPlaceholder : Maybe Item -> Position
getPositionFromPlaceholder maybe =
  case maybe of
    Nothing -> Position 0 0
    Just placeholder -> placeholder.position


updatePlaceholderDefinition : Maybe Item -> String -> Maybe Item
updatePlaceholderDefinition maybe newDefinition =
  case maybe of
    Nothing -> Nothing
    Just item -> Just {item | definition = newDefinition}

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
          on "change" targetValue (Signal.message address << Add)
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
      on "click" eventPos (Signal.message address << ShowPlaceholder)
    ]
    ((List.map itemView model.items) ++ [placeholderView model.placeholder])


itemView : Item -> Html
itemView item =
  div
    [
      class "placeholder item",
      style [
        ("position", "absolute"),
        ("background-color","whitesmoke"),
        ("top", toPxString item.position.y),
        ("left", toPxString item.position.x)]
    ]
    [
      text item.definition
    ]

placeholderView: Maybe Item -> Html
placeholderView maybe =
  case maybe of
    Just item -> itemView item
    Nothing -> text "click"

toPxString: a -> String
toPxString pos =
  (toString pos) ++ "px"
---- INPUTS ----

-- wire the entire application together
main : Signal Html
main =
  Signal.map (view actions.address) model

-- manage the model of our application over time
model : Signal Model
model =
  Signal.foldp update initialModel actions.signal


-- actions from user input
actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp


eventPos : Json.Decoder Position
eventPos =
  Json.object2
    Position
    ("clientX" := Json.int)
    ("clientY" := Json.int)