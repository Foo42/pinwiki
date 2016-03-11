module App where

import Html exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Signal, Address)

------ Models ------

type alias Model = {
  items : List Item,
  nextuid : Int,
  placeholder : Maybe Position
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
    placeholder = Nothing
  }

------ Updates ------

type Action
  = NoOp
  | ShowPlaceholder Position

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    ShowPlaceholder position -> model


------ View ------

view: Address Action -> Model -> Html
view address model =
  div
    [
      class "board",
      style [("position", "relative")]
    ]
    [
      placeholderView model.placeholder
    ]

placeholderView: Maybe Position -> Html
placeholderView maybe =
  case maybe of
    Just position ->
      div
        [
          class "placeholder item",
          style [("position", "absolute"), ("top", toString position.x), ("left", toString position.y)]
        ]
        [
          text "placeholder"
        ]

    Nothing -> text "click"

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
