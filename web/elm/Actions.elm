module Actions where

import Models exposing (..)

type Action
  = NoOp
  | ShowPlaceholder Position
  | DefinitionAccepted String
  | BeginEdit Int