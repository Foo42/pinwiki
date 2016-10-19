module Update exposing (..)

import Models exposing (..)
import Msg exposing (..)
import MyPorts

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (Debug.log "msg:" msg) of
    NoOp -> model ! []

    BeginEdit id ->
      let updateEditing item = {item | isEditing = (item.uid == id) }
      in
        { model |
          items = (List.map updateEditing model.items),
          placeholder = Nothing,
          editing = (findById id model.items)
        } ! [MyPorts.focus "#bottom-edit"]

    ShowPlaceholder position ->
      { model |
        placeholder = Just (Item "placeholder" position 0 True False),
        items = (stopEditing model.items)
      } ! [MyPorts.focus "#bottom-edit"]

    DefinitionAccepted definition ->
      case model.placeholder of
        Nothing -> {model | items = ((List.map (definitionChanged definition) model.items) |> stopEditing)} ! []
        Just placeholder ->
          let item = (Item definition placeholder.position model.nextuid False False)
          in
            { model |
              placeholder = Nothing,
              items = model.items ++ [item],
              nextuid = model.nextuid + 1
            } ! []

definitionChanged : String -> Item -> Item
definitionChanged newDefinition item =
  case item.isEditing of
    True -> {item | definition = newDefinition}
    False -> item

findById : Int -> List Item -> Maybe Item
findById targetId items =
  (List.filter (\item -> item.uid == targetId) items) |> List.head

updatePlaceholderDefinition : Maybe Item -> String -> Maybe Item
updatePlaceholderDefinition maybe newDefinition =
  case maybe of
    Nothing -> Nothing
    Just item -> Just {item | definition = newDefinition}

stopEditing : List Item -> List Item
stopEditing items =
  items
  |> List.map(\item -> {item | isEditing = False})