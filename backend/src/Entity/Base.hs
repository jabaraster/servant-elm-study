{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity.Base where

import Control.Lens.TH
import Data.Aeson.TH
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics

import Entity.Helper

data Id a = Id
  {_idValue :: Text}
  deriving (Generic, Show, Eq)
makeFields ''Id
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 3} ''Id)

data EntityMeta = EntityMeta
  { _entityMetaCreatedAt :: UTCTime
  , _entityMetaUpdatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq)
makeFields ''EntityMeta
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 11} ''EntityMeta)

data Entity d = Entity
  { _entityId :: Id d
  , _entityMeta :: EntityMeta
  , _entityPayload :: d
  }
  deriving (Generic, Show, Eq)

makeFields ''Entity
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 7} ''Entity)
