{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Entity.Authority where

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

data Authority = Authority
  { _authorityCreatedAt :: UTCTime
  , _authorityUpdatedAt :: UTCTime
  , _authorityLevel :: Integer
  , _authorityName :: Text
  }
  deriving (Generic, Show, Eq)
makeFields ''Authority
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 10} ''Authority)

type AuthorityRecord = Record Authority

instance FromAttributeValue AuthorityRecord where
  fromAttributeValue rec = do
    i <- DH.getInteger rec "id"
    ca <- DH.getUTCTime rec "createdAt"
    ua <- DH.getUTCTime rec "updatedAt"
    lv <- DH.getInteger rec "level"
    nm <- DH.getText rec "name"
    return $ Record (Id i) (Authority ca ua lv nm)
