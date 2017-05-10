module Components.PomodoroSession exposing (Msg)

import Components.Timer as Timer exposing(..)

type Msg
  = StartWork
  | CancelWork
  | CompleteWork
  | StartRest
  | CompleteRest


type alias = {
  workTimer = Timer.defaultModel
  restTimer = Timer.defaultModel
}

