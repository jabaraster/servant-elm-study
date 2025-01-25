{-# LANGUAGE TemplateHaskell #-}

module Model (
  User (..),
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Time.Calendar

data User = User
  { id :: Int
  , firstName :: String
  , lastName :: String
  , registrationDate :: Day
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''User)
