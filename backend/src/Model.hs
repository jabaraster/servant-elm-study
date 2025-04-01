{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model (
  User (..),
  Authority (..),
) where

import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Jabara.Amazonka.DynamoDB.Helper (FromAttributeValue)
import qualified Jabara.Amazonka.DynamoDB.Helper as DH

data User = User
  { id :: Integer
  , firstName :: Text
  , lastName :: Text
  , registrationDate :: UTCTime
  }
  deriving (Generic, Show, Eq)
instance ToJSON User
instance FromJSON User

instance FromAttributeValue User where
  fromAttributeValue rec = do
    i <- DH.getInteger rec "id"
    fn <- DH.getText rec "firstName"
    ln <- DH.getText rec "lastName"
    rd <- DH.getUTCTime rec "registrationDate"
    return $ User i fn ln rd

data Authority = Authority
  { createdAt :: UTCTime
  , updatedAt :: UTCTime
  , level :: Integer
  , name :: Text
  }
  deriving (Generic, Show, Eq)
instance ToJSON Authority
instance FromJSON Authority

instance FromAttributeValue Authority where
  fromAttributeValue rec = do
    ca <- DH.getUTCTime rec "createdAt"
    ua <- DH.getUTCTime rec "updatedAt"
    lv <- DH.getInteger rec "level"
    nm <- DH.getText rec "name"
    return $ Authority ca ua lv nm
