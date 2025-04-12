module Entity exposing (..)

import Time exposing (Posix)


type alias EntityMeta =
    { createdAt : Posix
    , updatedAt : Posix
    }
