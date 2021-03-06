module Components.PomodoroSession exposing (Msg(CompleteWork, CompleteRest, StartWork), Model, subscriptions, defaultModel, update, TimeSet)
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

type alias TimeSet =
  { workDuration: Time
  , restDuration: Time}

subscriptions: Sub Msg
subscriptions =
  Sub.batch [ Sub.map WorkTimerMsg Timer.subscriptions
  , Sub.map RestTimerMsg Timer.subscriptions]

defaultModel: TimeSet -> Model
defaultModel set =
  { workTimerModel = Timer.defaultModel
  , restTimerModel = Timer.defaultModel
  , workDuration = set.workDuration
  , restDuration = set.restDuration}

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
            Timer.CompleteTimer _ -> dispatch CompleteWork
            _ -> Cmd.none
      in
      { model | workTimerModel = newTimerModel} ! [nextCmd, Cmd.map WorkTimerMsg timerCmd]
    RestTimerMsg timerMsg ->
      let 
          (newTimerModel, timerCmd) = Timer.update timerMsg model.restTimerModel
          nextCmd = case timerMsg of
            Timer.CompleteTimer _ -> dispatch CompleteRest
            _ -> Cmd.none
      in
      { model | restTimerModel = newTimerModel} ! [Cmd.map RestTimerMsg timerCmd]

dispatch: msg -> Cmd msg
dispatch msg = 
  Task.perform (\_ -> msg) (Task.succeed 0)

