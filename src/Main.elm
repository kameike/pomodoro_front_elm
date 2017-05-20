import Html exposing (..)
import Html.Attributes exposing (..)
import Date exposing (fromTime)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Task exposing (Task)
import Components.Timer as Timer exposing (..)
import Components.PomodoroSession as Pomodoro exposing (..)

type Msg
  = PomodoroSession Pomodoro.Msg
type Flags = None

main : Program Never Model Msg
main =
  Html.program {init = init
               , update = update
               , view = view 
               , subscriptions = subscriptions }

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.map PomodoroSession Pomodoro.subscriptions

type alias Model = 
  { currentTime : Time
  , pomodoroModel: Pomodoro.Model}

emptyModel: Model
emptyModel = { currentTime = 0 
             , pomodoroModel = Pomodoro.defaultModel { workDuration = Time.second * 10 
                                                     , restDuration = Time.second * 10}}
init: (Model, Cmd Msg)
init =
  (emptyModel , Cmd.none)

view: Model -> Html Msg
view model = 
  let 
      currentView = 
        if model.pomodoroModel.workTimerModel.isActive then
         workingView model.pomodoroModel.workTimerModel
        else if model.pomodoroModel.restTimerModel.isActive then
         restingView model.pomodoroModel.restTimerModel
        else 
         div [] [text "TIMER IS NOT ACTIVE"]
  in
  div [] [currentView
         , button [onClick (PomodoroSession Pomodoro.StartWork)] [text "スタートポモドーロ"]]

restingView: Timer.Model -> Html Msg
restingView timerModel =
  div [] [
    div [] [ text "RESTING" ],
    div [] [ text (Timer.lastMinutes timerModel ++ ":" ++ Timer.lastSecounds timerModel) ]
  ]

workingView: Timer.Model -> Html Msg
workingView timerModel =
  div [] [
    div [] [ text "WORKING" ],
    div [] [ text (Timer.lastMinutes timerModel ++ ":" ++ Timer.lastSecounds timerModel) ]
  ]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    PomodoroSession msg ->
      let 
          (newModel, newCmd) = Pomodoro.update msg model.pomodoroModel
      in
      {model| pomodoroModel = newModel} ! [Cmd.map PomodoroSession newCmd]

