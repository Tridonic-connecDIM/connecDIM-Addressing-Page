import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing ((:=))
import String
import Task exposing (..)
import Graphics.Element exposing (show)

type alias AddressedDevice =
  { address : Int
  , types : List Int
  }

type alias Model =
  { mac : String
  , name : String
  , lines : List Int
  , lineNames : List String
  , addressedDevices : List AddressedDevice
  , addressing : Bool
  , error : String
  }

type Action = NoOp
            | EraseError
            | DisplayError String
            | StartAddressing
            | StopAddressing
            | AddDevice Int (List Int)
            | SetGatewayData String String (List Int) (List String)
            | UnaddressedState Int

main =
  Signal.map (view actions.address) model

update : Result String (Action) -> Model -> Model
update action model =
  case action of
    Ok a ->
      case a of
        NoOp ->
          model
        EraseError ->
          {model | error = ""}
        DisplayError e ->
          {model | error = e}
        StartAddressing ->
          {model | addressing = True}
        StopAddressing ->
          {model | addressing = False}
        AddDevice a t ->
          {model | addressedDevices = model.addressedDevices ++ [{address = a, types = t}]}
        SetGatewayData macAddr hostname activeLines lineNames'  ->
          {model | mac = macAddr, name = hostname, lines = activeLines, lineNames = lineNames'}
        UnaddressedState state ->
          if state == 0
          then {model | addressing = False}
          else model
    Err e -> {model | addressing = False, error = e}

model : Signal Model
model =
  Signal.foldp update { mac = ""
                      , name = ""
                      , lines = []
                      , lineNames = []
                      , addressedDevices = []
                      , addressing = False
                      , error = ""
                      } actions.signal

view : Signal.Address (Result String (Action)) -> Model -> Html
view address model =
  let deviceImages = List.map (\deviceType -> case deviceType of
                                                1 -> "emergency"
                                                2 -> "hid"
                                                3 -> "downlight"
                                                4 -> "incandescent"
                                                5 -> "converter"
                                                6 -> "led"
                                                7 -> "relay"
                                                8 -> "colour_control"
                                                254 -> "msensor"
                                                _ -> "fluoro")
      devicesDiv =
        List.map (\device -> div [] <| (List.map (\imgName -> img [src <| "/img/type_" ++ imgName ++ ".png"] []) <| deviceImages device.types) ++
                              [ text <| "Assigned address " ++ toString device.address ++ " to device"
                              ]) model.addressedDevices
  in
    div [myStyle] <|
      [ div [] [ text model.name ]
      , div [] [ text model.mac ]
      , div [] [ text model.error ]
      , button [ onClick query.address <| Encode.object [ ("method", Encode.string "setunaddressed"), ("params", Encode.list [Encode.int 1]) ] ] [ text "Start Addressing" ]
      , button [ onClick address <| Ok EraseError ] [ text "Clear Error" ]
      , button [ onClick query.address <| Encode.object [ ("method", Encode.string "readgateway"), ("params", Encode.list []) ] ] [ text "Stop Addressing" ]
      ]
      ++ devicesDiv

myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]

echoJson : Encode.Value -> Encode.Value
echoJson value =
  Encode.object [ ("method", Encode.string "echo"), ("params", Encode.list [ value ]) ]

query : Signal.Mailbox Encode.Value
query =
  Signal.mailbox <| Encode.object [ ("method", Encode.string "readgateway"), ("params", Encode.list []) ]

results : Signal.Mailbox (Result String (Action))
results =
  Signal.mailbox <| Err ""

actions : Signal.Mailbox (Result String (Action))
actions =
  Signal.mailbox <| Ok NoOp


port requests : Signal (Task x ())
port requests =
  Signal.map lookupGatewayMethod query.signal
  |> Signal.map (\task -> Task.map (always task) (Signal.send actions.address <| Ok EraseError) `andThen` Task.toResult `andThen` Signal.send actions.address)

lookupGatewayMethod : Encode.Value -> Task String (Action)
lookupGatewayMethod json =
  let encoded_json = Encode.encode 0 json
      toUrl =
        if encoded_json == "null"
          then fail "Null JSON is invalid"
          else succeed <| "/cgi-bin/json.cgi?json=" ++ encoded_json
  in
    toUrl `andThen` (Http.get gatewayResolve >> mapError (\x -> toString x))

gatewayResolve : Decode.Decoder (Action)
gatewayResolve =
  Decode.oneOf
    [ Decode.object1 DisplayError (Decode.at ["error", "message"] Decode.string)
    , Decode.object4 SetGatewayData (Decode.at ["result", "mac"] Decode.string) (Decode.at ["result", "hostname"] Decode.string) (Decode.at ["result", "activelines"] <| Decode.list Decode.int) (Decode.at ["result", "linenames"] <| Decode.list Decode.string)
    , Decode.object2 AddDevice (Decode.at ["result", "address"] Decode.int) (Decode.at ["result", "type"] <| Decode.list Decode.int)
    , Decode.object1 UnaddressedState (Decode.at ["result", "unaddressed"] Decode.int)
    ]
