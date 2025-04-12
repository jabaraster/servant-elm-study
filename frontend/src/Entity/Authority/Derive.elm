module Entity.Authority.Derive exposing (..)

import Entity.Authority exposing (..)
import Entity.Derive exposing (decodeAndMap, decodeEntityMeta, encodeEntityMeta)
import Json.Decode
import Json.Encode


encodeAuthorityId : AuthorityId -> Json.Encode.Value
encodeAuthorityId val =
    case val of
        AuthorityId { value } ->
            Json.Encode.object
                [ ( "value", Json.Encode.string value )
                ]


encodeAuthority : Authority -> Json.Encode.Value
encodeAuthority =
    \value0 ->
        Json.Encode.object [ ( "label", Json.Encode.string value0.label ), ( "level", Json.Encode.int value0.level ) ]


encodeAuthorityEntity : AuthorityEntity -> Json.Encode.Value
encodeAuthorityEntity =
    \value0 ->
        Json.Encode.object
            [ ( "id", encodeAuthorityId value0.id )
            , ( "meta", encodeEntityMeta value0.meta )
            , ( "payload", encodeAuthority value0.payload )
            ]


decodeAuthorityId : Json.Decode.Decoder AuthorityId
decodeAuthorityId =
    Json.Decode.andThen
        (Json.Decode.succeed << Entity.Authority.toId)
        (Json.Decode.field "value" Json.Decode.string)


decodeAuthority : Json.Decode.Decoder Authority
decodeAuthority =
    Json.Decode.succeed Authority
        |> decodeAndMap (Json.Decode.field "label" Json.Decode.string)
        |> decodeAndMap (Json.Decode.field "level" Json.Decode.int)


decodeAuthorityEntity : Json.Decode.Decoder AuthorityEntity
decodeAuthorityEntity =
    Json.Decode.succeed (\id meta payload -> { id = id, meta = meta, payload = payload })
        |> decodeAndMap (Json.Decode.field "id" decodeAuthorityId)
        |> decodeAndMap (Json.Decode.field "meta" decodeEntityMeta)
        |> decodeAndMap (Json.Decode.field "payload" decodeAuthority)


compareAuthorityId : AuthorityId -> AuthorityId -> Order
compareAuthorityId (AuthorityId lhs) (AuthorityId rhs) =
    (\lhs0 rhs0 -> compare lhs0.value rhs0.value) lhs rhs


compareAuthority : Authority -> Authority -> Order
compareAuthority =
    \lhs0 rhs0 ->
        case compare lhs0.label rhs0.label of
            EQ ->
                compare lhs0.level rhs0.level

            o0 ->
                o0


compareAuthorityEntity : AuthorityEntity -> AuthorityEntity -> Order
compareAuthorityEntity =
    \lhs0 rhs0 ->
        case compareAuthorityId lhs0.id rhs0.id of
            EQ ->
                compareAuthority lhs0.payload rhs0.payload

            o0 ->
                o0
