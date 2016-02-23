import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing ((:=))
import String
import Task exposing (..)
import Graphics.Element exposing (show)
import Maybe.Extra exposing (isJust, isNothing)
import Signal.Extra as Signal

type alias AddressedDevice =
  { address : Int
  , types : List Int
  }

type alias Model =
  { mac : String
  , name : String
  , addressingLine : Maybe Int
  , lines : List Int
  , lineNames : List String
  , addressedDevices : List AddressedDevice
  , addressing : Bool
  , unaddressedState : Maybe Bool
  , error : String
  , unusedAddresses : List Int
  }

type Action = NoOp
            | UnsetAddressingLine
            | SetAddressingLine Int
            | EraseError
            | DisplayError String
            | StartAddressing
            | StopAddressing
            | AddDevice Int (List Int)
            | SetGatewayData String String (List Int) (List String)
            | UnaddressedState Int
            | SetUnusedAddresses (List Int)

port title : String
port title = "Addressing Sorceress"

setUnaddressedQuery : Int -> Encode.Value
setUnaddressedQuery line =
  Encode.object [ ("method", Encode.string "setunaddressed"), ("params", Encode.list [Encode.int line]) ]

findUnaddressedQuery : Int -> Encode.Value
findUnaddressedQuery line =
  Encode.object [ ("method", Encode.string "findunaddressed"), ("params", Encode.list [Encode.int line]) ]

readGatewayQuery : Encode.Value
readGatewayQuery = Encode.object [ ("method", Encode.string "readgateway"), ("params", Encode.list []) ]

main =
  Signal.map (view actions.address) model

update : Action -> Model -> Model
update action model =
  case action of
    NoOp ->
      model
    UnsetAddressingLine ->
      { model
      | addressingLine = Nothing
      , unaddressedState = Nothing
      , unusedAddresses = []
      , addressedDevices = []
      , error = ""
      }
    SetAddressingLine line ->
      { model
      | addressingLine = Just line
      }
    EraseError ->
      { model
      | error = ""
      }
    DisplayError e ->
      { model
      | error = e
      }
    StartAddressing ->
      { model
      | addressing = True
      }
    StopAddressing ->
      { model
      | addressing = False
      }
    SetUnusedAddresses addresses ->
      { model
      | unusedAddresses = addresses
      }
    AddDevice a t ->
      if model.addressing
      then
        { model
        | addressedDevices = model.addressedDevices ++ [{address = a, types = t}]
        , unusedAddresses = List.drop 1 model.unusedAddresses
        }
      else
        model
    SetGatewayData macAddr hostname activeLines lineNames'  ->
      { model
      | mac = macAddr
      , name = hostname
      , lines = activeLines
      , lineNames = lineNames'
      }
    UnaddressedState state ->
      if state == 0
      then
        { model
        | unaddressedState = Just False
        , error = "There are no unaddressed devices"
        , addressing = False}
      else
        { model
        | unaddressedState = Just True
        }

-- The application's state
model : Signal Model
model =
  Signal.foldp update { mac = ""
                      , name = ""
                      , addressingLine = Nothing
                      , lines = []
                      , lineNames = []
                      , addressedDevices = []
                      , addressing = False
                      , unaddressedState = Nothing
                      , error = ""
                      , unusedAddresses = []
                      } actions.signal

-- Pair the line names with line numbers and filter out the inactive lines
activeLines : Model -> List (Int, String)
activeLines model =
  List.indexedMap
    (\index name -> (index + 1, name)) model.lineNames
  |> List.filter
    (\(line, name) -> List.member line model.lines)

deviceTypeToImageName : Int -> String
deviceTypeToImageName deviceType =
  case deviceType of
    1 -> "emergency"
    2 -> "hid"
    3 -> "downlight"
    4 -> "incandescent"
    5 -> "converter"
    6 -> "led"
    7 -> "relay"
    8 -> "colour_control"
    254 -> "msensor"
    _ -> "fluoro"

deviceTypesToImages : List Int -> List Html
deviceTypesToImages =
  List.map deviceTypeToImageName >> List.map (\imgName -> img [src <| "/img/type_" ++ imgName ++ ".png"] [])

devicesToDivList : List AddressedDevice -> List Html
devicesToDivList =
  List.map (\device -> div [] <| deviceTypesToImages device.types ++ [ text <| "Assigned address " ++ toString device.address ++ " to device" ])

view : Signal.Address (Action) -> Model -> Html
view address model =
  let returnButton = button [ onClick address <| UnsetAddressingLine ] [ text "Return" ]
      buttons = if isJust model.addressingLine
                then
                  case model.unaddressedState of
                    Just True ->
                      if model.addressing == False
                      then [ button [ onClick address <| StartAddressing ] [ text "Start Addressing" ], returnButton ]
                      else [ button [ onClick address <| StopAddressing ] [ text "Stop Addressing" ] ]
                    Just False ->
                      [ returnButton ]
                    Nothing ->
                      []
                else
                  List.map (\(line, name) -> button [onClick address <| SetAddressingLine line] [ text <| "Address " ++ name ++ " (" ++ toString line ++ ")" ]) (activeLines model)
      loadingWheel =
        if model.addressing
        || String.length model.mac == 0
        || isJust model.addressingLine && isNothing model.unaddressedState
        then
          [ img [ src "/img/loading.gif", width 20, height 20] [] ]
        else
          []
  in
    div [myStyle] <|
      [ div [] [ text model.name ]
      , div [] [ text model.mac ]
      , div [] [ text <|
        case model.addressingLine of
          Just a ->
            "Line " ++ toString a
          Nothing ->
            ""]
      , div [] [ text model.error ]
      ]
      ++ List.map (\item -> div [] [item]) (buttons ++ devicesToDivList model.addressedDevices ++ loadingWheel)

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

echoJson : Encode.Value -> Encode.Value
echoJson value =
  Encode.object [ ("method", Encode.string "echo"), ("params", Encode.list [ value ]) ]

query : Signal.Mailbox Encode.Value
query =
  Signal.mailbox <| readGatewayQuery

results : Signal.Mailbox (Action)
results =
  Signal.mailbox <| NoOp

actions : Signal.Mailbox (Action)
actions =
  Signal.mailbox <| NoOp

port requests : Signal (Task x ())
port requests =
  Signal.map lookupGatewayMethod query.signal
  |> Signal.map
    (\task -> taskMapReplace (Signal.send actions.address <| EraseError) task
    `andThen` Task.toResult
    `andThen` (resultToAction >> Signal.send actions.address))

port addressingAssistant : Signal (Task x ())
port addressingAssistant =
  Signal.map sendAddressingJsonBasedOnModel (Signal.dropRepeats (Signal.zip model actions.signal))

resultToAction : Result String Action -> Action
resultToAction result =
  case result of
    Ok action ->
      action
    Err e ->
      DisplayError e

taskMapReplace : Task x a -> b -> Task x b
taskMapReplace task returnValue =
  Task.map (\_ -> returnValue) task

sendAddressingJsonBasedOnModel : (Model, Action) -> Task a ()
sendAddressingJsonBasedOnModel (model, action) =
  let
    sendQuery a =
      case (Maybe.map (a >> Signal.send query.address) model.addressingLine) of
        Just task ->
          task
        Nothing ->
          succeed ()
  in
    case action of
      SetAddressingLine _ ->
        sendQuery findUnaddressedQuery
      AddDevice _ _ ->
        if model.addressing == True
        then sendQuery findUnaddressedQuery
        else succeed ()
      StartAddressing ->
        sendQuery setUnaddressedQuery
      UnaddressedState state ->
        if state /= 0 && model.addressing == True
        then sendQuery setUnaddressedQuery
        else succeed ()
      _ -> succeed ()

lookupGatewayMethod : Encode.Value -> Task String Action
lookupGatewayMethod json =
  let encodedJson = Encode.encode 0 json
      toUrl =
        if encodedJson == "null"
          then fail "Null JSON is invalid"
          else succeed <| "/cgi-bin/json.cgi?json=" ++ encodedJson
  in
    toUrl `andThen` (Http.get gatewayResolve >> mapError (\x -> toString x))

-- lineToUnusedAddresses :

gatewayResolve : Decode.Decoder Action
gatewayResolve =
  Decode.oneOf
    [ Decode.object1 DisplayError (Decode.at ["error", "message"] Decode.string)
    , Decode.object4 SetGatewayData (Decode.at ["result", "mac"] Decode.string) (Decode.at ["result", "hostname"] Decode.string) (Decode.at ["result", "activelines"] <| Decode.list Decode.int) (Decode.at ["result", "linenames"] <| Decode.list Decode.string)
    , Decode.object2 AddDevice (Decode.at ["result", "address"] Decode.int) (Decode.at ["result", "type"] <| Decode.list Decode.int)
    , Decode.object1 UnaddressedState (Decode.at ["result", "unaddressed"] Decode.int)
    ]
