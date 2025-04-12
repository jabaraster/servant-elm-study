module EntityTest exposing (..)

import Entity.User exposing (UserId(..))
import Entity.User.Derive as User
import Expect exposing (equal)
import Iso8601
import Json.Decode
import Json.Encode
import Test exposing (..)
import Time


suite : Test
suite =
    describe "JSON encode/decode"
        [ describe "User"
            [ test "decode UserId"
                (\_ ->
                    equal
                        (Ok (UserId { value = "hoge" }))
                        (Json.Decode.decodeString User.decodeUserId "{\"value\":\"hoge\"}")
                )
            , test "decode UserEntity"
                (\_ ->
                    equal
                        (Json.Encode.object
                            [ ( "id", Json.Encode.object [ ( "value", Json.Encode.string "hoge" ) ] )
                            , ( "meta"
                              , Json.Encode.object
                                    [ ( "createdAt", Iso8601.encode <| Time.millisToPosix 0 )
                                    , ( "updatedAt", Iso8601.encode <| Time.millisToPosix 100 )
                                    ]
                              )
                            , ( "payload"
                              , Json.Encode.object
                                    [ ( "firstName", Json.Encode.string "jabara" )
                                    , ( "lastName", Json.Encode.string "ster" )
                                    ]
                              )
                            ]
                        )
                        (User.encodeUserEntity
                            { id = UserId { value = "hoge" }
                            , meta =
                                { createdAt = Time.millisToPosix 0
                                , updatedAt = Time.millisToPosix 100
                                }
                            , payload = { firstName = "jabara", lastName = "ster" }
                            }
                        )
                )
            ]
        ]
