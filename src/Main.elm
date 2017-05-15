import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Date exposing (fromTime)
import Task exposing (Task)
import Components.Timer as Timer exposing (..)
import Components.PomodoroSession as Pomodoro exposing (..)

main : Program Never Model Msg
main =
  Html.program {init = init
               , update = update
               , view = view 
               , subscriptions = subscriptions }

type Msg
  = TimerMsg Timer.Msg
  | PomodoroSession Pomodoro.Msg

type Flags = None

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.map TimerMsg Timer.subscriptions

type alias Model = 
  { currentTime : Time
  , timerModel: Timer.Model }

emptyModel: Model
emptyModel = { currentTime = 0 
             , timerModel = Timer.defaultModel}

init: (Model, Cmd Msg)
init =
  (emptyModel, Cmd.none)

view: Model -> Html Msg
view model = 
  let 
      timerModel = model.timerModel
  in
     timerView timerModel


timerView: Timer.Model -> Html Msg
timerView timerModel =
     div [] [
       div [] [text (
         if timerModel.isActive then
           "timer : "
           ++  Timer.lastMinutes timerModel
           ++ ":"
           ++ Timer.lastSecounds timerModel
         else
           "not-active"
         )],
       div [] [text (toString (Timer.progressOf timerModel))],
       button [onClick (TimerMsg (Timer.activateMsgFor { duration = (Time.minute * 25) }))] [text "10秒タイマー"],
       div [] [text ("end")]
     ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    TimerMsg msg ->
      let
          (newTimerModel, timerCmd) = Timer.update msg model.timerModel
          newModel = {model | timerModel = newTimerModel}
      in
         newModel ! [Cmd.map TimerMsg timerCmd]
    PomodoroSession msg ->
      model ! [Cmd.none]


