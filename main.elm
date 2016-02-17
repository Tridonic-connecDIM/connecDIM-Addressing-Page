import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Encode as JEncode exposing (..)
import Json.Decode as JDecode exposing ((:=))
import String
import Task exposing (..)
import Graphics.Element exposing (..)
import Maybe.Extra exposing (..)
import Regex exposing (..)



view : (Result String (String)) -> Html
view result =
        value =
        case result of
            Err msg ->
                fromElement <| show reg
            Ok reply ->
                fromElement
                <| show
                <| List.concat
                <| List.map maybeToList
                <| List.concat
                <| List.map .submatches (find All (regex <| (escape "<body>") ++ "\n(.*)" ++ (escape "</body>")) reply)
    in
        div []
        [ button [ onClick query.address <| JEncode.object [("method", JEncode.string "readgateway"), ("params", JEncode.list [])] ] [ text "Read Gateway" ]
        , value
        ]

main =
  Signal.map view results.signal

query : Signal.Mailbox JEncode.Value
query =
  Signal.mailbox <| JEncode.object []

results : Signal.Mailbox (Result String (String))
results =
    Signal.mailbox (Err "")

port requests : Signal (Task x ())
port requests =
    Signal.map lookupGatewayMethod query.signal
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)

lookupGatewayMethod : JEncode.Value -> Task String (String)
lookupGatewayMethod json =
    let encoded_json = JEncode.encode 0 json
        toUrl =
            if String.length encoded_json > 2
            then succeed <| "/cgi-bin/json.cgi?json=" ++ encoded_json
            else fail "Click the button, my dude"
    in
        toUrl `andThen` (mapError (\x -> toString x) << Http.getString)
