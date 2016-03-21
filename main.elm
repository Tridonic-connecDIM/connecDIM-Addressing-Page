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
import Gateway exposing (Action(..))
import Tridonic
import Color
import Result.Extra as Result

type alias Model = Gateway.GatewayModel {}

port title : String
port title =
  "Main"

main =
  Signal.map2 view model Window.dimensions

update : Time -> Action -> Model -> Model
update timeStamp action model =
  case action of
    NoOp ->
      model
    EraseError ->
      { model | error = ""
      }
    DisplayError e ->
      { model | error = e
      }
    DisplayHelpText e ->
      { model | helpText = e
      }
    SetGatewayData macAddr hostname activeLines lineNames'  ->
      { model | mac = macAddr,
                name = hostname,
                lines = activeLines,
                lineNames = lineNames'
      }
    _ -> model

initialModel : Model
initialModel =
  { mac = ""
  , name = ""
  , lines = []
  , lineNames = []
  , error = ""
  , helpText = ""
  }

-- The application's state
model : Signal Model
model =
  Time.timestamp actions.signal
  |> Signal.foldp (uncurry update) initialModel

view : Model -> (Int, Int) -> Element
view model (windowWidth, windowHeight) =
  let pageHeader = Tridonic.pageHeader (windowWidth, windowHeight) "Main"
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

port requests : Signal (Task x ())
port requests =
  Gateway.sendGatewayRequest query.signal gatewayDecoder actions.address

gatewayDecoder : Decoder Action
gatewayDecoder =
  Decode.oneOf
    [ Decode.object1 DisplayError (Decode.at ["error", "message"] Decode.string)
    , Decode.object4 SetGatewayData (Decode.at ["result", "mac"] Decode.string) (Decode.at ["result", "hostname"] Decode.string) (Decode.at ["result", "activelines"] <| Decode.list Decode.int) (Decode.at ["result", "linenames"] <| Decode.list Decode.string)
    ]
