{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Entity.Base where

import Control.Lens.TH
import Data.Aeson.TH
import GHC.Generics

import Entity.Helper

data Id a = Id
  {_idValue :: Integer}
  deriving (Generic, Show, Eq)

makeFields ''Id
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 3} ''Id)

data Record d = Record
  { _recordId :: Id d
  , _recordPayload :: d
  }
  deriving (Generic, Show, Eq)

makeFields ''Record
$(deriveJSON defaultOptions {fieldLabelModifier = fieldModifier 7} ''Record)
