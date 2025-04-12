module Entity.Authority exposing (..)

import Entity exposing (EntityMeta)


type AuthorityId
    = AuthorityId { value : String }


fromId : AuthorityId -> String
fromId (AuthorityId { value }) =
    value


toId : String -> AuthorityId
toId value =
    AuthorityId { value = value }


type alias Authority =
    { label : String
    , level : Int
    }


type alias AuthorityEntity =
    { id : AuthorityId
    , meta : EntityMeta
    , payload : Authority
    }
