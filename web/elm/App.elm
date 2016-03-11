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
    placeholder = Nothing,
    bottomRight = {x = 1000, y = 1000}
  }

------ Updates ------

type Action
  = NoOp
  | ShowPlaceholder Position

update : Action -> Model -> Model
update action model =
  case action of
    NoOp -> model
    ShowPlaceholder position -> {model | placeholder = Just position}


------ View ------

view: Address Action -> Model -> Html
view address model =
  div
    [
      class "board",
      style [
        ("position", "relative"),
        ("background-color","whitesmoke"),
        ("width", toString model.bottomRight.x ++ "px"),
        ("height", toString model.bottomRight.y ++ "px")
      ],
      on "click" eventPos (Signal.message address << ShowPlaceholder)
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
          style [("position", "absolute"), ("top", toPxString position.y), ("left", toPxString position.x)]
        ]
        [
          text "placeholder"
        ]

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