{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Entity.User where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Jabara.Amazonka.DynamoDB.Helper (FromAttributeValue)
import qualified Jabara.Amazonka.DynamoDB.Helper as DH

import Entity.Base
import Entity.Helper

data User = User
  { _userCreatedAt :: UTCTime
  , _userUpdatedAt :: UTCTime
  , _userFirstName :: Text
  , _userLastName :: Text
  , _userRegistrationDate :: UTCTime
  }
  deriving (Generic, Show, Eq)
makeFields ''User
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 5} ''User)

type UserEntity = Entity User

instance FromAttributeValue UserEntity where
  fromAttributeValue rec = do
    i <- DH.getText rec "id"
    ca <- DH.getUTCTime rec "createdAt"
    ua <- DH.getUTCTime rec "updatedAt"
    fn <- DH.getText rec "firstName"
    ln <- DH.getText rec "lastName"
    rd <- DH.getUTCTime rec "registrationDate"
    return $ Entity (Id i) (User ca ua fn ln rd)
