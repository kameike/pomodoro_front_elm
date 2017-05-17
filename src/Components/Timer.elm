module Components.Timer exposing(Msg(..), Model, TimerData, subscriptions, update, activateMsgFor, defaultModel, lastMinutes, lastSecounds, progressOf)
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

activateMsgFor: TimerData -> Msg
activateMsgFor = ActivateTimer

subscriptions: Sub Msg
subscriptions = 
  Sub.batch [ Time.every (Time.millisecond * 100) UpdateTimer ]

defaultModel: Model
defaultModel = { startTime = Nothing
               , currentTimerData = Nothing
               , currentTime = 0
               , isActive = False }

lastMinutes: Model -> String
lastMinutes model =
  let 
      lastTime = lastTimeOf model
      minutes = 
        if lastTime > 0 then
          floor(lastTimeOf model / Time.minute)
        else
          0
  in 
     attachZeroTo minutes

lastSecounds: Model -> String
lastSecounds model = 
  let 
      lastTime = lastTimeOf model
      seconds = 
        if lastTime > 0 then
          floor (toFloat(floor(lastTimeOf model) % floor(Time.minute)) / Time.second)
        else
          0
  in 
     attachZeroTo seconds

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
      if model.isActive then
         (model, Cmd.none)
      else
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
                lastTimeOf model < 0
              Nothing ->
                False
          hasFinish =
            case model.currentTimerData of
              Just _ -> False
              Nothing -> True
          isActive = not isCompleted && not hasFinish
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
         ]
      else
         (newModel, Cmd.none)
    _ ->
      (model, Cmd.none)

