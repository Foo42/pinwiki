module App where

import Html exposing (..)

import Models exposing (..)
import Actions exposing (..)
import Update exposing (..)
import View exposing (view)


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
