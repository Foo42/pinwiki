module View exposing (..)

import Models exposing (..)
import Msg exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import String exposing (join)

------ View ------
view: Model -> Html Msg
view model =
  div
    []
    [
      (boardView model),
      inputView model
    ]

inputView: Model -> Html Msg
inputView model =
  div
    [
      style [("width", "100%"), ("position","fixed"), ("bottom", "0")]
    ]
    [
      input
        [
          id "bottom-edit",
          style [("width", "100%") ],
          (placeholder "type..."),
          (autofocus True),
          (value (Maybe.withDefault "" (Maybe.andThen (itemBeingEdited model.items) (Just << .definition)))),
          on "change" (Json.object1 DefinitionAccepted targetValue)
        ]
        []
    ]

boardView: Model -> Html Msg
boardView model =
  div
    [
      class "board",
      style [
        ("position", "relative"),
        ("width", toString model.bottomRight.x ++ "px"),
        ("height", toString model.bottomRight.y ++ "px")
      ]
      , on "click" (Json.object1 ShowPlaceholder eventPos)
    ]
    ((List.map (itemView) model.items) ++ [placeholderView model.placeholder])

itemView : Item -> Html Msg
itemView item =
  div
    [
      class (itemClasses item),
      draggable "true",
      style [
        ("position", "absolute"),
        ("background-color","whitesmoke"),
        ("top", toPxString item.position.y),
        ("left", toPxString item.position.x)]
      , onWithOptions "click" {stopPropagation = True, preventDefault = True} (Json.succeed (BeginEdit item.uid))
    ]
    [
      text item.definition,
      div [
        class "item-control delete-item-control"
        ]
        [text "delete"]
    ]

itemClasses : Item -> String
itemClasses item =
  [("placeholder", item.uid == 0), ("item", True), ("being-edited", item.isEditing)]
  |> List.filter snd
  |> List.map fst
  |> String.join " "

placeholderView : Maybe Item -> Html Msg
placeholderView maybe =
  case maybe of
    Just item -> itemView item
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