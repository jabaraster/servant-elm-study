module Api exposing (..)

import Entity.Authority exposing (AuthorityEntity)
import Entity.Authority.Derive as Authority
import Entity.User exposing (UserEntity)
import Entity.User.Derive as User
import Http exposing (Error(..))
import Json.Decode


getAuthorities : (Result Http.Error (List AuthorityEntity) -> msg) -> Cmd msg
getAuthorities handler =
    Http.get
        { url = "/api/authorities"
        , expect = Http.expectJson handler <| Json.Decode.list Authority.decodeAuthorityEntity
        }


getUsers : (Result Http.Error (List UserEntity) -> msg) -> Cmd msg
getUsers handler =
    Http.get
        { url = "/api/users"
        , expect = Http.expectJson handler <| Json.Decode.list User.decodeUserEntity
        }


errorText : Http.Error -> String
errorText err =
    case err of
        BadUrl url ->
            "Bad URL: " ++ url

        Timeout ->
            "Timeout"

        NetworkError ->
            "Network error"

        BadStatus status ->
            "Bad status: " ++ String.fromInt status

        BadBody s ->
            "Bad body: " ++ s
