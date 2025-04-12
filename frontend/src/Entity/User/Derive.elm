module Entity.User.Derive exposing (..)

import Entity.Derive exposing (decodeAndMap, decodeEntityMeta)
import Entity.User exposing (..)
import Json.Decode
import Json.Encode


encodeUserId : UserId -> Json.Encode.Value
encodeUserId val =
    case val of
        UserId { value } ->
            Json.Encode.object
                [ ( "value", Json.Encode.string value )
                ]


encodeUser : User -> Json.Encode.Value
encodeUser =
    \value0 ->
        Json.Encode.object
            [ ( "firstName", Json.Encode.string value0.firstName ), ( "lastName", Json.Encode.string value0.lastName ) ]


encodeUserEntity : UserEntity -> Json.Encode.Value
encodeUserEntity =
    \value0 -> Json.Encode.object [ ( "id", encodeUserId value0.id ), ( "payload", encodeUser value0.payload ) ]


decodeUserId : Json.Decode.Decoder UserId
decodeUserId =
    Json.Decode.andThen
        (Json.Decode.succeed << Entity.User.toId)
        (Json.Decode.field "value" Json.Decode.string)


decodeUser : Json.Decode.Decoder User
decodeUser =
    Json.Decode.succeed User
        |> decodeAndMap (Json.Decode.field "firstName" Json.Decode.string)
        |> decodeAndMap (Json.Decode.field "lastName" Json.Decode.string)


decodeUserEntity : Json.Decode.Decoder UserEntity
decodeUserEntity =
    Json.Decode.succeed UserEntity
        |> decodeAndMap (Json.Decode.field "id" decodeUserId)
        |> decodeAndMap (Json.Decode.field "meta" decodeEntityMeta)
        |> decodeAndMap (Json.Decode.field "payload" decodeUser)


compareUserId : UserId -> UserId -> Order
compareUserId (UserId lhs) (UserId rhs) =
    (\lhs0 rhs0 -> compare lhs0.value rhs0.value) lhs rhs


compareUser : User -> User -> Order
compareUser =
    \lhs0 rhs0 ->
        case compare lhs0.firstName rhs0.firstName of
            EQ ->
                compare lhs0.lastName rhs0.lastName

            o0 ->
                o0


compareUserEntity : UserEntity -> UserEntity -> Order
compareUserEntity =
    \lhs0 rhs0 ->
        case compareUserId lhs0.id rhs0.id of
            EQ ->
                compareUser lhs0.payload rhs0.payload

            o0 ->
                o0


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe f encodeMaybeValue =
    case encodeMaybeValue of
        Nothing ->
            Json.Encode.null

        Just justValue ->
            f justValue
