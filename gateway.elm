module Gateway
  ( echoJson
  , activeLines
  , setUnaddressedQuery
  , findUnaddressedQuery
  , findAllUnaddressedQuery
  , readLineQuery
  , readGatewayQuery
  , queryGatewayWithMethod
  , sendGatewayRequest
  , Action(..)
  , GatewayModel
  ) where

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Http
import Task exposing (Task)
import Maybe
import Result.Extra as Result

type Action = NoOp
            | UnsetAddressingLine
            | SetAddressingLine Int
            | SetEdaliClass (Maybe Int)
            | EraseError
            | DisplayError String
            | DisplayHelpText String
            | StartAddressing
            | StopAddressing
            | AddDevice Int (List Int)
            | AddEdaliDevice Int
            | SetGatewayData String String (List Int) (List String)
            | UnaddressedState Int
            | SetUnusedAddresses (List Int)
            | DropOneUnusedAddress

type alias GatewayModel a =
  { a | mac : String,
        name : String,
        lines : List Int,
        lineNames : List String,
        error : String,
        helpText : String
  }

echoJson : Encode.Value -> Encode.Value
echoJson value =
  Encode.object [ ("method", Encode.string "echo"), ("params", Encode.list [ value ]) ]

-- Pair the line names with line numbers and filter out the inactive lines
activeLines : List Int -> List String -> List (Int, String)
activeLines lines lineNames =
  List.indexedMap
    (\index name -> (index + 1, name)) lineNames
  |> List.filter
    (\(line, _) -> List.member line lines)

queryGatewayWithMethod : Encode.Value -> Decoder a -> Task String a
queryGatewayWithMethod json decoder =
  "/cgi-bin/json.cgi?json=" ++ (Encode.encode 0 json)
  |> Http.get decoder
  |> Task.mapError toString

genericLineQuery : String -> Int -> Encode.Value
genericLineQuery method line =
  Encode.object [ ("method", Encode.string method), ("params", Encode.list [Encode.int line]) ]

setUnaddressedQuery : Int -> Maybe Int -> Maybe Int -> Encode.Value
setUnaddressedQuery line address edaliClass =
  Maybe.oneOf
    [ Maybe.map2 (\address edaliClass -> Encode.object [ ("method", Encode.string "edalisetunaddressed"), ("params", Encode.list [ Encode.int line, Encode.int edaliClass, Encode.int address ]) ]) address edaliClass
    , Maybe.map (\address -> Encode.object [ ("method", Encode.string "setunaddressed"), ("params", Encode.list [ Encode.int line, Encode.int address ]) ]) address
    , Maybe.map (\edaliClass -> Encode.object [ ("method", Encode.string "edalisetunaddressed"), ("params", Encode.list [ Encode.int line, Encode.int edaliClass ]) ]) edaliClass
    ] |> Maybe.withDefault (genericLineQuery "setunaddressed" line)

findUnaddressedQuery : Int -> Encode.Value
findUnaddressedQuery = genericLineQuery "findunaddressed"

findAllUnaddressedQuery : Int -> Encode.Value
findAllUnaddressedQuery = genericLineQuery "findallunaddressed"

readLineQuery : Int -> Encode.Value
readLineQuery = genericLineQuery "readline"

readGatewayQuery : Encode.Value
readGatewayQuery = Encode.object [ ("method", Encode.string "readgateway"), ("params", Encode.list []) ]

resolveRequest : Signal (Signal.Address Action) -> Signal (Task String Action) -> Signal (Task a ())
resolveRequest =
  Signal.map2 (\address task -> Task.map (\_ -> task) (Signal.send address EraseError)
                                `Task.andThen` Task.toResult
                                `Task.andThen` (Result.extract DisplayError >> Signal.send address))

sendGatewayRequest : Signal Encode.Value -> Decoder Action -> Signal.Address Action -> Signal (Task a ())
sendGatewayRequest query decoder address =
  Signal.map2 queryGatewayWithMethod query (Signal.constant decoder)
  |> resolveRequest (Signal.constant address)
