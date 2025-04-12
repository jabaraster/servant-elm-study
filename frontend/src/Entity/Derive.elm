module Entity.Derive exposing
    ( decodeAndMap
    , decodeEntityMeta
    )

import Entity exposing (EntityMeta)
import Iso8601
import Json.Decode


decodeEntityMeta : Json.Decode.Decoder EntityMeta
decodeEntityMeta =
    Json.Decode.succeed (\createdAt updatedAt -> { createdAt = createdAt, updatedAt = updatedAt })
        |> decodeAndMap (Json.Decode.field "createdAt" Iso8601.decoder)
        |> decodeAndMap (Json.Decode.field "updatedAt" Iso8601.decoder)


decodeAndMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
decodeAndMap =
    Json.Decode.map2 (|>)
