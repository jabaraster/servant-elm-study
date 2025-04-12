module Entity.Authority.Derive exposing (..)

import Array
import Dict
import Entity.Authority exposing (..)
import Entity.Derive exposing (decodeAndMap, decodeEntityMeta)
import Json.Decode
import Json.Encode
import Set


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
        Json.Encode.object [ ( "id", encodeAuthorityId value0.id ), ( "payload", encodeAuthority value0.payload ) ]


decodeAuthorityId : Json.Decode.Decoder AuthorityId
decodeAuthorityId =
    Json.Decode.andThen
        (Json.Decode.succeed << Entity.Authority.toId)
        (Json.Decode.field "value" Json.Decode.string)


decodeAuthority : Json.Decode.Decoder Authority
decodeAuthority =
    Json.Decode.succeed (\label level -> { label = label, level = level })
        |> decodeAndMap (Json.Decode.field "label" Json.Decode.string)
        |> decodeAndMap (Json.Decode.field "level" Json.Decode.int)


decodeAuthorityEntity : Json.Decode.Decoder AuthorityEntity
decodeAuthorityEntity =
    Json.Decode.succeed AuthorityEntity
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


encodeMaybe : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeMaybe f encodeMaybeValue =
    case encodeMaybeValue of
        Nothing ->
            Json.Encode.null

        Just justValue ->
            f justValue


encodeChar : Char -> Json.Encode.Value
encodeChar value =
    Json.Encode.string (String.fromChar value)


encodeResult : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
encodeResult errEncoder okEncoder value =
    case value of
        Err err ->
            Json.Encode.object [ ( "$", Json.Encode.string "Err" ), ( "a", errEncoder err ) ]

        Ok ok ->
            Json.Encode.object [ ( "$", Json.Encode.string "Ok" ), ( "a", okEncoder ok ) ]


decodeChar : Json.Decode.Decoder Char
decodeChar =
    Json.Decode.andThen
        (\str ->
            case String.toList str of
                [ c ] ->
                    Json.Decode.succeed c

                _ ->
                    Json.Decode.fail "decodeChar: too many charactors for Char type"
        )
        Json.Decode.string


decodeResult : Json.Decode.Decoder err -> Json.Decode.Decoder ok -> Json.Decode.Decoder (Result err ok)
decodeResult errDecoder okDecoder =
    Json.Decode.andThen
        (\tag ->
            case tag of
                "Err" ->
                    Json.Decode.map Err (Json.Decode.field "a" errDecoder)

                "Ok" ->
                    Json.Decode.map Ok (Json.Decode.field "a" okDecoder)

                _ ->
                    Json.Decode.fail ("decodeResult: Invalid tag name: " ++ tag)
        )
        (Json.Decode.field "$" Json.Decode.string)


compareList : (a -> a -> Order) -> List a -> List a -> Order
compareList f lhs rhs =
    case ( lhs, rhs ) of
        ( [], [] ) ->
            EQ

        ( x :: xs, [] ) ->
            GT

        ( [], y :: ys ) ->
            LT

        ( x :: xs, y :: ys ) ->
            case f x y of
                EQ ->
                    compareList f xs ys

                ret ->
                    ret


compareMaybe : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
compareMaybe f lhs rhs =
    case ( lhs, rhs ) of
        ( Nothing, Nothing ) ->
            EQ

        ( Nothing, Just _ ) ->
            GT

        ( Just _, Nothing ) ->
            LT

        ( Just x, Just y ) ->
            f x y


compareBool : Bool -> Bool -> Order
compareBool lhs rhs =
    case ( lhs, rhs ) of
        ( False, False ) ->
            EQ

        ( False, True ) ->
            LT

        ( True, False ) ->
            GT

        ( True, True ) ->
            EQ


compareSet : (comparable -> comparable -> Order) -> Set.Set comparable -> Set.Set comparable -> Order
compareSet f lhs rhs =
    compareList f (Set.toList lhs) (Set.toList rhs)


compareArray : (a -> a -> Order) -> Array.Array a -> Array.Array a -> Order
compareArray f lhs rhs =
    compareList f (Array.toList lhs) (Array.toList rhs)


compareDict : (a -> a -> Order) -> Dict.Dict comparable a -> Dict.Dict comparable a -> Order
compareDict f lhs rhs =
    compareList (\ls rs -> compareTuple compare f ls rs) (Dict.toList lhs) (Dict.toList rhs)


compareTuple : (a -> a -> Order) -> (b -> b -> Order) -> ( a, b ) -> ( a, b ) -> Order
compareTuple f g ( la, lb ) ( ra, rb ) =
    case f la ra of
        EQ ->
            g lb rb

        ord ->
            ord


compareResult : (err -> err -> Order) -> (ok -> ok -> Order) -> Result err ok -> Result err ok -> Order
compareResult f g lhs rhs =
    case ( lhs, rhs ) of
        ( Err l, Err r ) ->
            f l r

        ( Err _, _ ) ->
            LT

        ( _, Err _ ) ->
            GT

        ( Ok l, Ok r ) ->
            g l r
