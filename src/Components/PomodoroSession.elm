module Components.PomodoroSession exposing (Msg)

import Components.Timer as Timer exposing(..)
import Time exposing (Time)
import Task exposing (Task)

type alias Duration = Time

type Msg
  = StartWork
  | CancelWork
  | CompleteWork
  | StartRest
  | StopRest
  | CompleteRest
  | WorkTimerMsg Timer.Msg
  | RestTimerMsg Timer.Msg

type alias Model = 
  { workTimerModel : Timer.Model
  , restTimerModel : Timer.Model
  , workDuration: Duration
  , restDuration: Duration}


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartWork ->
      let 
          timerActivateMsg = (Timer.activateMsgFor { duration = model.workDuration })
          (newTimerModel, timerCmd) = Timer.update timerActivateMsg model.workTimerModel
      in
      { model | workTimerModel = newTimerModel } ! [Cmd.map WorkTimerMsg timerCmd]
    CancelWork ->
      let 
        (newTimerModel, timerCmd) = Timer.update Timer.CancelTimer model.workTimerModel
      in
      { model | workTimerModel = newTimerModel } ! [Cmd.map WorkTimerMsg timerCmd]
    CompleteWork ->
      model ! [dispatch StartRest]
    StartRest ->
      let 
          timerActivateMsg = (Timer.activateMsgFor { duration = model.restDuration })
          (newTimerModel, timerCmd) = Timer.update timerActivateMsg model.restTimerModel
      in
      { model | restTimerModel = newTimerModel } ! [Cmd.map RestTimerMsg timerCmd]
    StopRest ->
      let 
        (newTimerModel, timerCmd) = Timer.update Timer.CancelTimer model.restTimerModel
      in
      { model | restTimerModel = newTimerModel } ! [Cmd.map RestTimerMsg timerCmd]
    CompleteRest ->
      (model, Cmd.none)
    WorkTimerMsg timerMsg ->
      let 
          (newTimerModel, timerCmd) = Timer.update timerMsg model.workTimerModel
          nextCmd = case timerMsg of
            Timer.CompletedTimer _ -> dispatch CompleteWork
            _ -> Cmd.none
      in
      { model | workTimerModel = newTimerModel} ! [nextCmd, Cmd.map WorkTimerMsg timerCmd]
    RestTimerMsg timerMsg ->
      let 
          (newTimerModel, timerCmd) = Timer.update timerMsg model.restTimerModel
          nextCmd = case timerMsg of
            Timer.CompletedTimer _ -> dispatch CompleteRest
            _ -> Cmd.none
      in
      { model | restTimerModel = newTimerModel} ! [Cmd.map RestTimerMsg timerCmd]

dispatch: msg -> Cmd msg
dispatch msg = 
  Task.perform (\_ -> msg) (Task.succeed 0)

