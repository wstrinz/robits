module RobitGame where

import Html exposing (div, button, text, br)
import Signal exposing (Address, (<~), (~))
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled, style)
import Graphics.Element exposing (show)
import Time exposing (..)
import StartApp

type Action = Scavenge | Combine | Tick Time

type alias Model =  {parts: Float, robits: Float, scavengeTimeout: Float}

modState : {parts: Float, robits: Float, scavengeTimeout: Float}
modState = {parts = 0, robits = 0, scavengeTimeout = 0}

type alias App model action =
    { model : model
    , view : Address action -> model -> Html.Html
    , update : action -> model -> model
    }

tick : Signal (Maybe Action)
tick = Signal.map
       (\t -> Just (Tick t))
       (every second)

start : App modState Action -> Signal Html.Html
start app =
  let
    actions =
      Signal.mailbox Nothing

    address =
      Signal.forwardTo actions.address Just

    mod_op =
      Signal.foldp
        (\(Just action) modState -> app.update action modState)
        app.model
        (Signal.merge actions.signal tick)
  in
    Signal.map (app.view address) mod_op

main =
  start { model = modState, view = view, update = update }

combineDiv address model = button [ onClick address Combine,
                                    disabled (not (canCombine model)),
                                    buttonStyle ]
                                  [ text "Combine" ]

scavengeDiv address model = button [ onClick address Scavenge,
                                     buttonStyle,
                                     disabled (actionDisabled Scavenge model) ]
                                   [ text (scavengeText model) ]

textDiv model = div [] [ text "Parts: ",
               text (toString model.parts),
               text " Robits: ",
               text (toString model.robits) ]

controlDiv address model = div []
                               [(scavengeDiv address model),
                                (combineDiv address model)]

view : Address Action -> Model -> Html.Html
view address model =
  div []
    [ textDiv model,
      br [] [],
      controlDiv address model ]

buttonStyle = style [("margin-right", "5px"),
                     ("padding", "10px")]


update : Action -> Model -> Model
update action model =
  case action of
    Scavenge -> scavenge action model
    Combine -> combine action model
    Tick t -> doTick action model

scavenge : Action -> Model -> Model
scavenge action model = {model | parts <- model.parts + 3,
                                 scavengeTimeout <- 10}

combine : Action -> Model -> Model
combine action model = {model | parts <- model.parts - 2,
                                robits <- model.robits + 1}

doTick : Action -> Model -> Model
doTick (Tick t) model = {model | scavengeTimeout <- if model.scavengeTimeout > 0
                                                      then model.scavengeTimeout - 1
                                                      else 0}

canCombine model = model.parts > 1

actionDisabled Scavenge model = model.scavengeTimeout > 0

scavengeText model = if actionDisabled Scavenge model
                       then "Scavenge (" ++ (toString model.scavengeTimeout) ++ ")"
                       else "Scavenge"
