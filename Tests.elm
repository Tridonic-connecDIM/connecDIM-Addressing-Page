module Tests where

import ElmTest exposing (..)

import String
import Gateway


all : Test
all =
    suite "A Test Suite"
        [ test "Filter active gateway lines" (assertEqual ([(1, "Line 1")]) (Gateway.activeLines [1] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" (assertEqual ([(2, "Line 2")]) (Gateway.activeLines [2] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" (assertEqual ([(2, "")]) (Gateway.activeLines [2] ["Line 1", "", "Line 3", "Line 4"]))
        , test "Filter active gateway lines" (assertEqual ([]) (Gateway.activeLines [] ["Line 1", "Line 2", "Line 3", "Line 4"]))
        , test "Addition" (assertEqual (3 + 7) 10)
        , test "String.left" (assertEqual "a" (String.left 1 "abcdefg"))
        ]
