module Gateway
  ( echoJson
  , activeLines
  , setUnaddressedQuery
  , findUnaddressedQuery
  , findAllUnaddressedQuery
  , readLineQuery
  , readGatewayQuery
  , queryGatewayWithMethod
  ) where

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Http
import Task exposing (Task)
import Maybe

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
  let encodedJson = Encode.encode 0 json
      toUrl =
        if encodedJson == "null"
          then Task.fail "Null JSON is invalid"
          else Task.succeed <| "/cgi-bin/json.cgi?json=" ++ encodedJson
  in
    toUrl `Task.andThen` (Http.get decoder >> Task.mapError (\x -> toString x))

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
