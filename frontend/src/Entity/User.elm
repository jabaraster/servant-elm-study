module Entity.User exposing (..)


type UserId
    = UserId { value : String }


fromId : UserId -> String
fromId (UserId { value }) =
    value


toId : String -> UserId
toId value =
    UserId { value = value }


type alias User =
    { firstName : String
    , lastName : String
    }


type alias UserEntity =
    { id : UserId
    , payload : User
    }
