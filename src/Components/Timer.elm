module Components.Timer exposing(Msg, Model, TimerData, subscriptions, update, activateWith, defaultModel, lastMinutes, lastSecounds, progressOf)
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


defaultModel: Model
defaultModel = { startTime = Nothing
               , currentTimerData = Nothing
               , currentTime = 0
               , isActive = False }

lastMinutes: Model -> String
lastMinutes model =
  let minutes = floor(lastTimeOf model / Time.minute)
  in attachZeroTo minutes

lastSecounds: Model -> String
lastSecounds model = 
  let seconds = floor (toFloat(floor(lastTimeOf model) % floor(Time.minute)) / Time.second)
  in attachZeroTo seconds

lastTimeOf: Model -> Float
lastTimeOf model =
  let
      startTime = Maybe.withDefault 0 model.startTime
      timerData = Maybe.withDefault { duration = 0 } model.currentTimerData
  in
  timerData.duration - model.currentTime + startTime

attachZeroTo: Int -> String
attachZeroTo num =
  if num < 10 then
    "0" ++ (toString num)
  else
    toString num

progressOf: Model -> Float
progressOf model = 
  let
      timerData = Maybe.withDefault { duration = 0 } model.currentTimerData
      duration = timerData.duration
      lastTime = lastTimeOf model
      retio = lastTime / duration
  in
  if retio > 1 then 0
  else if retio < 0 then 1
  else 1 - retio

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

