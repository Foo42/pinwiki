module Models exposing (..)

type alias Model = {
  items : List Item,
  nextuid : Int,
  bottomRight : Position,
  placeholder : Maybe Item,
  editing: Maybe Item
}

type alias Item = {
  definition : String,
  position : Position,
  uid : Int,
  isEditing : Bool,
  isSelected: Bool
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
    bottomRight = {x = 1000, y = 1000},
    editing = Nothing
  }
