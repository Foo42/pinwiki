import Html.App

import Models exposing (initialModel)
import Update exposing (update)
import View exposing (view)

-- main = Html.App.beginnerProgram { model = initialModel, update = update, view = view}
main = Html.App.program {init = (initialModel ! []), update = update , subscriptions  = ( \_ -> Sub.none), view  = view }