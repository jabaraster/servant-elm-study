{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity.Base where

import Control.Lens.TH
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics

import Entity.Helper

data Id a = Id
  {_idValue :: Text}
  deriving (Generic, Show, Eq)

makeFields ''Id
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 3} ''Id)

data Entity d = Entity
  { _recordId :: Id d
  , _recordPayload :: d
  }
  deriving (Generic, Show, Eq)

makeFields ''Entity
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 7} ''Entity)
