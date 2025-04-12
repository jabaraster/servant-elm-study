module Entity.Derive exposing
    ( decodeAndMap
    , decodeEntityMeta
    , encodeEntityMeta
    )

import Entity exposing (EntityMeta)
import Iso8601
import Json.Decode
import Json.Encode


encodeEntityMeta : EntityMeta -> Json.Encode.Value
encodeEntityMeta =
    \value0 ->
        Json.Encode.object
            [ ( "createdAt", Json.Encode.string <| Iso8601.fromTime <| value0.createdAt )
            , ( "updatedAt", Json.Encode.string <| Iso8601.fromTime <| value0.updatedAt )
            ]


decodeEntityMeta : Json.Decode.Decoder EntityMeta
decodeEntityMeta =
    Json.Decode.succeed (\createdAt updatedAt -> { createdAt = createdAt, updatedAt = updatedAt })
        |> decodeAndMap (Json.Decode.field "createdAt" Iso8601.decoder)
        |> decodeAndMap (Json.Decode.field "updatedAt" Iso8601.decoder)


decodeAndMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
decodeAndMap =
    Json.Decode.map2 (|>)
