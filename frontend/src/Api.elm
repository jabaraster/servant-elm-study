module Api exposing
    ( AuthenticationTokens
    , errorText
    , getAuthorities
    , getUsers
    )

import Entity.Authority exposing (AuthorityEntity)
import Entity.Authority.Derive as Authority
import Entity.User exposing (UserEntity)
import Entity.User.Derive as User
import Http exposing (Error(..), Expect)
import Json.Decode


type alias AuthenticationTokens =
    { idToken : String
    , accessToken : String
    }


getAuth : { url : String, tokens : AuthenticationTokens, expect : Expect msg } -> Cmd msg
getAuth { url, tokens, expect } =
    Http.request
        { url = url
        , headers =
            [ authHeader tokens
            ]
        , expect = expect
        , method = "GET"
        , body = Http.emptyBody
        , timeout = Nothing
        , tracker = Nothing
        }


authHeader : AuthenticationTokens -> Http.Header
authHeader tokens =
    Http.header "Authorization" ("Bearer " ++ tokens.accessToken)


getAuthorities : AuthenticationTokens -> (Result Http.Error (List AuthorityEntity) -> msg) -> Cmd msg
getAuthorities tokens handler =
    getAuth
        { tokens = tokens
        , url = "/api/authorities"
        , expect = Http.expectJson handler <| Json.Decode.list Authority.decodeAuthorityEntity
        }


getUsers : AuthenticationTokens -> (Result Http.Error (List UserEntity) -> msg) -> Cmd msg
getUsers tokens handler =
    getAuth
        { tokens = tokens
        , url = "/api/users"
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
