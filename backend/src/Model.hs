{-# LANGUAGE DeriveGeneric #-}

module Model (
  User (..),
  Authority (..),
) where

import Data.Aeson
import Data.Time.Calendar
import GHC.Generics

data User = User
  { id :: Int
  , firstName :: String
  , lastName :: String
  , registrationDate :: Day
  }
  deriving (Generic, Show, Eq)
instance ToJSON User
instance FromJSON User

data Authority = Authority
  { createdAt :: String
  , updatedAt :: String
  , level :: Int
  , name :: String
  }
  deriving (Generic, Show, Eq)
instance ToJSON Authority
instance FromJSON Authority
