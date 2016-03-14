module Tests where

import ElmTest exposing (..)

import String
import Gateway
import List

all : Test
all =
    suite "A Test Suite"
      <| List.concat
      [ [ test "Filter active gateway lines" ([(1, "Line 1")] `assertEqual` (Gateway.activeLines [1] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([(2, "Line 2")] `assertEqual` (Gateway.activeLines [2] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([(1, "Line 1"), (2, "Line 2"), (3, "Line 3"), (4, "Line 4")] `assertEqual` (Gateway.activeLines [1, 2, 3, 4] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([(2, "")] `assertEqual` (Gateway.activeLines [2] ["Line 1", "", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([] `assertEqual` (Gateway.activeLines [2] []))
        , test "Filter active gateway lines" ([] `assertEqual` (Gateway.activeLines [2] ["Line 1"]))
        , test "Filter active gateway lines" ([] `assertEqual` (Gateway.activeLines [] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        ]
      , [ test "Filter active gateway lines" ([(1, "Line 1")] `assertEqual` (Gateway.activeLines [1] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([(2, "Line 2")] `assertEqual` (Gateway.activeLines [2] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([(1, "Line 1"), (2, "Line 2"), (3, "Line 3"), (4, "Line 4")] `assertEqual` (Gateway.activeLines [1, 2, 3, 4] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([(2, "")] `assertEqual` (Gateway.activeLines [2] ["Line 1", "", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" ([] `assertEqual` (Gateway.activeLines [2] []))
        , test "Filter active gateway lines" ([] `assertEqual` (Gateway.activeLines [2] ["Line 1"]))
        , test "Filter active gateway lines" ([] `assertEqual` (Gateway.activeLines [] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        ]
      ]
