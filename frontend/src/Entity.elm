module Entity exposing (..)

import Time exposing (Posix)


type alias EntityMeta =
    { createdAt : Posix
    , updatedAt : Posix
    }


type alias Entity i p =
    { id : i
    , meta : EntityMeta
    , payload : p
    }
