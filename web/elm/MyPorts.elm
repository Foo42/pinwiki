port module MyPorts exposing (..)
import Msg exposing (..)

port jsAlert : String -> Cmd msg
port focus : String -> Cmd msg
