import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, (:=))
import String
import Task exposing (..)
import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Signal.Extra as Signal
import Dict exposing (Dict)
import Time exposing (Time)
import Window
import Gateway
import Tridonic
import Color

type alias Model =
  { mac : String
  , name : String
  , lines : List Int
  , lineNames : List String
  , error : String
  , helpText : String
  , windowSize : (Int, Int)
  }

type Action = NoOp
            | EraseError
            | DisplayError String
            | DisplayHelpText String
            | SetGatewayData String String (List Int) (List String)
            | UpdateWindowSize (Int, Int)

port title : String
port title =
  "Main"

main =
  Signal.map view model

update : Time -> Action -> Model -> Model
update timeStamp action model =
  case action of
    NoOp ->
      model
    EraseError ->
      { model
      | error = ""
      }
    DisplayError e ->
      { model
      | error = e
      }
    DisplayHelpText e ->
      { model
      | helpText = e
      }
    SetGatewayData macAddr hostname activeLines lineNames'  ->
      { model
      | mac = macAddr
      , name = hostname
      , lines = activeLines
      , lineNames = lineNames'
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
                      , lines = []
                      , lineNames = []
                      , error = ""
                      , helpText = ""
                      , windowSize = (0,0)
                      } <| Time.timestamp actions.signal

view : Model -> Element
view model =
  let windowWidth = fst model.windowSize
      windowHeight = snd model.windowSize
      pageHeader = Tridonic.pageHeader model.windowSize "Main"
      centeredContainer = flip (container windowWidth) middle
  in
    [ pageHeader
      |> centeredContainer (heightOf pageHeader)
    , show model.lineNames
      |> centeredContainer (heightOf <| show model.lineNames)
    ] |> flow down

query : Signal.Mailbox Encode.Value
query =
  Signal.mailbox Gateway.readGatewayQuery

actions : Signal.Mailbox Action
actions =
  Signal.mailbox NoOp

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

gatewayResolve : Decoder Action
gatewayResolve =
  Decode.oneOf
    [ Decode.object1 DisplayError (Decode.at ["error", "message"] Decode.string)
    , Decode.object4 SetGatewayData (Decode.at ["result", "mac"] Decode.string) (Decode.at ["result", "hostname"] Decode.string) (Decode.at ["result", "activelines"] <| Decode.list Decode.int) (Decode.at ["result", "linenames"] <| Decode.list Decode.string)
    ]
