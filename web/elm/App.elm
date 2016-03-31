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
  uid : Int,
  isEditing : Bool
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
  | DefinitionAccepted String
  | BeginEdit Int

update : Action -> Model -> Model
update action model =
  case (Debug.log "action:" action) of
    NoOp -> model

    BeginEdit id ->
      let updateEditing item = {item | isEditing = (item.uid == id) }
      in
        { model |
          items = (List.map updateEditing model.items),
          placeholder = Nothing
        }

    ShowPlaceholder position ->
      {model | placeholder = Just (Item "placeholder" position 0 False)}

    DefinitionAccepted definition ->
      case model.placeholder of
        Nothing -> {model | items = (List.map (definitionChanged definition) model.items)}
        Just placeholder ->
          let item = (Item definition placeholder.position model.nextuid False)
          in
            { model |
              placeholder = Nothing,
              items = model.items ++ [item],
              nextuid = model.nextuid + 1
            }

definitionChanged : String -> Item -> Item
definitionChanged newDefinition item =
  case item.isEditing of
    True -> {item | definition = newDefinition}
    False -> item


itemBeingEdited : List Item -> Maybe Item
itemBeingEdited items =
  List.head (List.filter .isEditing items)

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
      class "placeholder item",
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

placeholderView : Address Action -> Maybe Item -> Html
placeholderView address maybe =
  case maybe of
    Just item -> itemView address item
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
  Signal.foldp update initialModel (Signal.map (Debug.watch "Actions") actions.signal)



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