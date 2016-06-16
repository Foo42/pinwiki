module Update where

import Models exposing (..)
import Actions exposing (..)

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
      { model |
        placeholder = Just (Item "placeholder" position 0 True),
        items = (stopEditing model.items)
      }

    DefinitionAccepted definition ->
      case model.placeholder of
        Nothing -> {model | items = ((List.map (definitionChanged definition) model.items) |> stopEditing)}
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


updatePlaceholderDefinition : Maybe Item -> String -> Maybe Item
updatePlaceholderDefinition maybe newDefinition =
  case maybe of
    Nothing -> Nothing
    Just item -> Just {item | definition = newDefinition}

stopEditing : List Item -> List Item
stopEditing items =
  items
  |> List.map(\item -> {item | isEditing = False})