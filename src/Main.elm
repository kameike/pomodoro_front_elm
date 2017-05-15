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
  = PomodoroSession Pomodoro.Msg

type Flags = None

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.map PomodoroSession Pomodoro.subscriptions

type alias Model = 
  { currentTime : Time
  , pomodoroModel: Pomodoro.Model}

emptyModel: Model
emptyModel = { currentTime = 0 
             , pomodoroModel = Pomodoro.defaultModel { workDuration = Time.second * 2
                                                     , restDuration = Time.second * 3}}
init: (Model, Cmd Msg)
init =
  (emptyModel , Cmd.none)

view: Model -> Html Msg
view model = 
  div [] [ timerView model.pomodoroModel.workTimerModel
         , timerView model.pomodoroModel.restTimerModel
         , button [onClick (PomodoroSession Pomodoro.StartWork)] [text "10秒タイマー"]]

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
       div [] [text ("end")]
     ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    PomodoroSession msg ->
      let 
          (newModel, newCmd) = Pomodoro.update msg model.pomodoroModel
      in
      {model| pomodoroModel = newModel} ! [Cmd.map PomodoroSession newCmd]


