module Components.Timer exposing(Msg, Model, TimerData,subscriptions, update, activateWith)
import Html exposing (..)
import Time exposing (Time)
import Task exposing (Task)


type alias Duration = Time
type alias CurrentTime = Time

type alias TimerData = {
  duration: Duration
}

type alias Model = {
  startTime: Maybe Time,
  currentTimerData: Maybe TimerData,
  currentTime: Time,
  isActive: Bool
}

type Msg
  = ActivateTimer TimerData
  | StartTimer CurrentTime 
  | UpdateTimer CurrentTime 
  | CompletedTimer CurrentTime
  | CancelTimer

activateWith: TimerData -> Msg
activateWith = ActivateTimer

subscriptions: Sub Msg
subscriptions = 
  Time.every (Time.millisecond * 100) UpdateTimer

update: Msg -> Model -> (Model , Cmd Msg)
update msg model =
  case msg of
    ActivateTimer data ->
      ({model 
      | currentTimerData = Just data 
      }, Task.perform StartTimer Time.now)
    StartTimer currentTime ->
      { model 
      | isActive = True
      , startTime = Just currentTime
      , currentTime = currentTime
      } ! []
    UpdateTimer currentTime ->
      let 
          isCompleted = 
            case model.currentTimerData of
              Just timerData ->
                (Maybe.withDefault 0 model.startTime) + timerData.duration < currentTime
              Nothing ->
                False
          isActive = not isCompleted && model.isActive
          newModel = { model 
                     | currentTime = currentTime
                     , currentTimerData = 
                        if isActive then
                          model.currentTimerData
                        else
                          Nothing
                     , isActive = isActive}
      in
      if isCompleted then
         newModel!
         [ case model.currentTimerData of
            Just timerData -> 
              Task.perform CompletedTimer (Task.succeed currentTime)
            Nothing -> Cmd.none
         , Cmd.none ]
      else
         (newModel, Cmd.none)
    CompletedTimer currentTime ->
      (model, Cmd.none)
    CancelTimer ->
      (model, Cmd.none)

