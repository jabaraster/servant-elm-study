module Api (
  usersHandler,
) where

import Data.Time.Calendar

import Model

usersHandler :: IO [User]
usersHandler =
  return
    [ User 1 "Isaac, " "Newton" (fromGregorian 1683 3 1)
    , User 2 "Albert, " "Einstein" (fromGregorian 1905 12 1)
    , User 3 "jabara, " "star" (fromGregorian 1975 9 29)
    , User 4 "Rakuda, " "amou" (fromGregorian 2010 3 3)
    ]
