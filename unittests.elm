import ElmTest exposing  (..)
import String exposing (..)

tests = suite "My Test Suite"
        [ test "Addition" (assertEqual (3 + 7) 10)
        , test "String.reverse" (assertEqual "ekiM" (String.reverse "Mike"))
        , test "This test should pass" (assert True)
        , test "This test should fail" (assert False)
        ]

main =
  elementRunner tests
