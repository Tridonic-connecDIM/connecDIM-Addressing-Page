import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))
import String
import Task exposing (..)
import Graphics.Element exposing (show)
import Maybe.Extra exposing (isJust, isNothing)
import Signal.Extra as Signal
import Dict exposing (Dict)
import Time exposing (Time)
import Window
import Gateway
import Tridonic

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
  , helpText : String
  , unusedAddresses : List Int
  , addressingStart : Maybe Time
  , addressingEnd : Maybe Time
  , windowSize : (Int, Int)
  }

type Action = NoOp
            | UnsetAddressingLine
            | SetAddressingLine Int
            | EraseError
            | DisplayError String
            | DisplayHelpText String
            | StartAddressing
            | StopAddressing
            | AddDevice Int (List Int)
            | SetGatewayData String String (List Int) (List String)
            | UnaddressedState Int
            | SetUnusedAddresses (List Int)
            | DropOneUnusedAddress
            | UpdateWindowSize (Int, Int)

port title : String
port title = "Addressing Sorceress"

errorStringsForRetry : List String
errorStringsForRetry =
  [ "The given address already has a device assigned to it"
  ]

errorStringForCompletion : String
errorStringForCompletion =
  "There are no unaddressed devices on the DALI line"

main =
  Signal.map (view actions.address) model

update : Time -> Action -> Model -> Model
update timeStamp action model =
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
      , addressingStart = Nothing
      , addressingEnd = Nothing
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
      let errorModel =
        { model
        | error = e
        }
      in
        if List.member e errorStringsForRetry
        then
          update
            timeStamp
            DropOneUnusedAddress
            model
        else
          if e == errorStringForCompletion
          then
            update
              timeStamp
              StopAddressing
              errorModel
          else
            errorModel
    DisplayHelpText e ->
      { model
      | helpText = e
      }
    StartAddressing ->
      { model
      | addressing = True
      , addressingStart = Just timeStamp
      , addressingEnd = Nothing
      }
    StopAddressing ->
      { model
      | addressing = False
      , addressingEnd = Just timeStamp
      }
    SetUnusedAddresses addresses ->
      { model
      | unusedAddresses = addresses
      }
    DropOneUnusedAddress ->
      { model
      | unusedAddresses = List.drop 1 model.unusedAddresses
      }
    AddDevice a t ->
      if model.addressing
      then
        update
          timeStamp
          DropOneUnusedAddress
          { model
          | addressedDevices = model.addressedDevices ++ [{address = a, types = t}]
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
        update
          timeStamp
          (DisplayError errorStringForCompletion)
          { model
          | unaddressedState = Just False
          }
      else
        { model
        | unaddressedState = Just True
        }
    UpdateWindowSize size ->
      { model
      | windowSize = size
      }

-- The application's state
model : Signal Model
model =
  Signal.foldp (uncurry update) { mac = ""
                      , name = ""
                      , addressingLine = Nothing
                      , lines = []
                      , lineNames = []
                      , addressedDevices = []
                      , addressing = False
                      , unaddressedState = Nothing
                      , error = ""
                      , helpText = ""
                      , unusedAddresses = []
                      , addressingStart = Nothing
                      , addressingEnd = Nothing
                      , windowSize = (0,0)
                      } <| Time.timestamp actions.signal

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
  List.map deviceTypeToImageName
  >> List.map (\imgName -> img [src <| "/img/type_" ++ imgName ++ ".png"] [])

devicesToDivList : List AddressedDevice -> List Html
devicesToDivList =
  List.map (\device -> div [] <| deviceTypesToImages device.types ++ [ text <| "Assigned address " ++ toString device.address ++ " to device" ])

view : Signal.Address (Action) -> Model -> Html
view address model =
  let returnButton = button [ onClick address UnsetAddressingLine ] [ text "Return" ]
      buttons = if isJust model.addressingLine
                then
                  case model.unaddressedState of
                    Just True ->
                      if model.addressing == False
                      then [ button [ onClick address StartAddressing ] [ text "Start Addressing" ], returnButton ]
                      else [ button [ onClick address StopAddressing ] [ text "Stop Addressing" ] ]
                    Just False ->
                      [ returnButton ]
                    Nothing ->
                      []
                else
                  Gateway.activeLines model.lines model.lineNames
                  |> List.map (\(line, name) -> button [onClick address <| SetAddressingLine line] [ text <| "Address " ++ name ++ " (" ++ toString line ++ ")" ])
      loadingWheel =
        if model.addressing
        || String.length model.mac == 0
        || isJust model.addressingLine && isNothing model.unaddressedState
        then
          [ img [ src "/img/loading.gif", width 20, height 20] [] ]
        else
          []
      lineName =
        case model.addressingLine of
          Just a ->
            "Line " ++ toString a
          Nothing ->
            ""
      mintuesToOneDecimalPlaceString =
        toString << \s -> (toFloat s / 60 * 10 |> truncate |> toFloat) / 10
      addressingTime =
        Maybe.withDefault ""
        <| Maybe.map (Time.inSeconds >> round >> mintuesToOneDecimalPlaceString >> \s -> "The last addressing session took " ++ s ++ " minutes") <| Maybe.map2 (-) model.addressingEnd model.addressingStart
  in
    div []
      [ Tridonic.pageHeader model.windowSize "Addressing"
      , div [textStyle] <|
        ([model.name
        , model.mac
        , lineName ] |> List.map (\item -> div [] [ text item ]))
        ++
        [ div [] [ text addressingTime ]
        , div [ style [ ("color", "red") ] ] [ text model.error ]
        ]
        ++
        List.map (\item -> div [ style [ ("color", "#118BD8") ] ] [ item ]) (buttons ++ devicesToDivList model.addressedDevices ++ loadingWheel)
        ++
        [ div [ style [ ("color", "#74C3DB") ] ] [ text model.helpText ] ]
      ]

textStyle : Attribute
textStyle =
  style
    [ ("color", "#073D8B")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

query : Signal.Mailbox Encode.Value
query =
  Signal.mailbox <| Gateway.readGatewayQuery

actions : Signal.Mailbox Action
actions =
  Signal.mailbox <| NoOp

port windowSizeUpdate : Signal (Task x ())
port windowSizeUpdate =
  Signal.map UpdateWindowSize Window.dimensions
  |> Signal.map (Signal.send actions.address)

port requests : Signal (Task x ())
port requests =
  Signal.map2 Gateway.queryGatewayWithMethod query.signal (Signal.constant gatewayResolve)
  |> Signal.map
    (\task -> Task.map (\_ -> task) (Signal.send actions.address EraseError)
    `andThen` Task.toResult
    `andThen` ((\result ->
                case result of
                  Ok action ->
                    action
                  Err e ->
                    DisplayError e) >> Signal.send actions.address))

port addressingAssistant : Signal (Task String ())
port addressingAssistant =
  Signal.map sendAddressingJsonBasedOnModel (Signal.dropRepeats (Signal.zip model actions.signal))

sendAddressingJsonBasedOnModel : (Model, Action) -> Task String ()
sendAddressingJsonBasedOnModel (model, action) =
  let
    sendQuery a =
      case (Maybe.map (a >> Signal.send query.address) model.addressingLine) of
        Just task ->
          task
        Nothing ->
          succeed ()
    setUnaddressedQuery' =
      flip Gateway.setUnaddressedQuery <| List.head model.unusedAddresses
    sendSetUnaddressedQuery =
        if model.addressing == True
        then sendQuery setUnaddressedQuery'
        else succeed ()
  in
    case action of
      SetAddressingLine _ ->
        sendQuery Gateway.findUnaddressedQuery
      AddDevice _ _ ->
        sendSetUnaddressedQuery
      StartAddressing ->
        sendQuery Gateway.readLineQuery
      SetUnusedAddresses xs ->
        sendSetUnaddressedQuery
      DisplayError e ->
        if List.member e errorStringsForRetry
        then
          sendSetUnaddressedQuery
        else
          succeed ()
      _ -> succeed ()

gatewayResolve : Decoder Action
gatewayResolve =
  Decode.oneOf
    [ Decode.object1 DisplayError (Decode.at ["error", "message"] Decode.string)
    , Decode.object4 SetGatewayData (Decode.at ["result", "mac"] Decode.string) (Decode.at ["result", "hostname"] Decode.string) (Decode.at ["result", "activelines"] <| Decode.list Decode.int) (Decode.at ["result", "linenames"] <| Decode.list Decode.string)
    , Decode.object2 AddDevice (Decode.at ["result", "address"] Decode.int) (Decode.at ["result", "type"] <| Decode.list Decode.int)
    , Decode.object1 UnaddressedState (Decode.at ["result", "unaddressed"] Decode.int)
    , Decode.object1 ((\addresses -> List.filter (not << flip List.member addresses) [0..63]) >> SetUnusedAddresses) (Decode.at ["result", "address"] <| Decode.list <| "number" := Decode.int)
    ]