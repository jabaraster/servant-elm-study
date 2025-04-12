module Entity.User.Derive exposing (..)

import Array
import Dict
import Entity.User exposing (..)
import Json.Decode
import Json.Encode
import Set


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
    Json.Decode.succeed (\firstName lastName -> { firstName = firstName, lastName = lastName })
        |> decodeAndMap (Json.Decode.field "firstName" Json.Decode.string)
        |> decodeAndMap (Json.Decode.field "lastName" Json.Decode.string)


decodeUserEntity : Json.Decode.Decoder UserEntity
decodeUserEntity =
    Json.Decode.succeed (\id payload -> { id = id, payload = payload })
        |> decodeAndMap (Json.Decode.field "id" decodeUserId)
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


decodeAndMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
decodeAndMap =
    Json.Decode.map2 (|>)


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
