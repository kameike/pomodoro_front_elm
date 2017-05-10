import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Date exposing (fromTime)
import Task exposing (Task)
import Components.Timer as Timer exposing (..)


main : Program Never Model Msg
main =
    Html.program {init = init
                         , update = update
                         , view = view 
                         , subscriptions = subscriptions }

type Msg
  = TimeMsg TimeMsg
  | TimerMsg Timer.Msg

type TimeMsg 
  = Tick Time
  | UpdateTime

type Flags = None

subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch 
  [Time.every Time.second (\_ -> UpdateTime |> TimeMsg)
  ,Sub.map TimerMsg Timer.subscriptions]

type alias Model = 
  { currentTime : Time
  , timerModel: Timer.Model }

emptyModel: Model
emptyModel =
  { currentTime = 0 
  , timerModel
  = { startTime = Nothing
    , currentTimerData = Nothing
    , currentTime = 0
    , isActive = False }}

init: (Model, Cmd Msg)
init =
  (emptyModel, Task.perform TimeMsg (Task.succeed UpdateTime))


view: Model -> Html Msg
view model = 
  let 
      currentTime = toString (fromTime model.currentTime)
      startTime = Maybe.withDefault 0 model.timerModel.startTime
      timerData = Maybe.withDefault { duration = 0 } model.timerModel.currentTimerData
      lastTime = timerData.duration - model.timerModel.currentTime + startTime
      statusMInt = floor(lastTime / Time.minute)
      statusSInt = floor(toFloat(floor(lastTime) % floor(Time.minute)) / Time.second)
      statusM = toString(statusMInt)
      statusS = toString(statusSInt)
  in
     div [] [
       div [] [text ("現在時刻: " ++ currentTime)],
       button [onClick (TimeMsg UpdateTime)] [text "更新ボタン"],
       div [] [text (
         if model.timerModel.isActive then
           "timer : " ++  statusM ++ ":" ++ statusS
         else
           "not-active"
         )],
       button [onClick (TimerMsg (Timer.activateWith { duration = (Time.minute * 10) }))] [text "10秒タイマー"],
       div [] [text ("end")]
     ]


update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    TimeMsg timeMsg ->
      case timeMsg of
        UpdateTime ->
          (model, Cmd.map TimeMsg (Task.perform Tick Time.now))
        Tick time ->
          ({model | currentTime = time}, Cmd.none)
    TimerMsg msg ->
      let
          (newTimerModel, timerCmd) = Timer.update msg model.timerModel
          newModel = {model | timerModel = newTimerModel}
      in
         newModel ! [Cmd.map TimerMsg timerCmd]

