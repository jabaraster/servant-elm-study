module Example exposing (..)

import Entity.User exposing (UserId(..))
import Entity.User.Derive as User
import Expect
import Json.Decode
import Test exposing (..)


suite : Test
suite =
    test "sample test"
        (\_ ->
            Expect.equal
                (Ok (UserId { value = "hoge" }))
                (Json.Decode.decodeString User.decodeUserId "{\"value\":\"hoge\"}")
        )
