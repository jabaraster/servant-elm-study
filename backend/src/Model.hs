{-# LANGUAGE DeriveGeneric #-}

module Model (
  User (..),
  Authority (..),
) where

import Data.Text (Text)
import Data.Aeson
import Data.Time.Calendar
import GHC.Generics

data User = User
  { id :: Integer
  , firstName :: Text
  , lastName :: Text
  , registrationDate :: Day
  }
  deriving (Generic, Show, Eq)
instance ToJSON User
instance FromJSON User

data Authority = Authority
  { createdAt :: Text
  , updatedAt :: Text
  , level :: Integer
  , name :: Text
  }
  deriving (Generic, Show, Eq)
instance ToJSON Authority
instance FromJSON Authority
