module Msg exposing (..)

import Models exposing (..)

type Msg
  = NoOp
  | ShowPlaceholder Position
  | DefinitionAccepted String
  | BeginEdit Int