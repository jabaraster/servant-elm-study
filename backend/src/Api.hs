{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Api (
  Db,
  getDb,
  usersHandler,
  userHandler,
  authoritiesHandler,
  authortyHandler,
) where

import Amazonka as AWS
import Amazonka.DynamoDB.GetItem
import Amazonka.DynamoDB.Scan
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.Prelude (fromList)
import Config
import Control.Exception.Safe (throwString)
import Control.Lens
import Data.Text (Text)
import qualified Data.Text as Text
import Jabara.Amazonka.DynamoDB.Helper (FromAttributeValue)
import qualified Jabara.Amazonka.DynamoDB.Helper as DH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Name (..), nameBase)
import System.Environment
import System.IO (stdout)

import Config
import Entity
import Entity.Authority
import Entity.User

data EntityTypes = EntityTypes
  { _entityTypesUser :: Text
  , _entityTypesAuthority :: Text
  }
  deriving (Show, Eq)
makeFields ''EntityTypes

entityTypes :: EntityTypes
entityTypes =
  EntityTypes
    { _entityTypesUser = "User"
    , _entityTypesAuthority = "Authority"
    }

data Db = Db
  { _dbEnv :: Env
  , _dbAppTable :: Text
  }
makeFields ''Db

getDb :: Config -> IO Db
getDb config = do
  logger <- AWS.newLogger (if config ^. runtimeEnv == Dev then AWS.Debug else AWS.Info) stdout
  discoveredEnv <- AWS.newEnv AWS.discover

  let env = discoveredEnv {AWS.logger = logger, AWS.region = AWS.Tokyo}
  return $ Db env ("servant-elm-study-" <> Text.pack (show $ config ^. runtimeEnv))

getById ::
  (FromAttributeValue a) =>
  Text ->
  Db ->
  Id b ->
  IO (Maybe a)
getById entityType db id_ = do
  let req =
        (newGetItem (db ^. appTable))
          & getItem_key
            .~ fromList
              [ ("entityType", S entityType)
              , ("id", S $ id_ ^. value)
              ]
  res <- AWS.runResourceT $ AWS.send (db ^. env) req
  case res ^. getItemResponse_item of
    Nothing -> return Nothing
    Just rec -> do
      ret <- DH.fromAttributeValueUnsafe rec
      return $ Just ret

list ::
  (FromAttributeValue a) =>
  Text ->
  Db ->
  IO [a]
list entityType db = do
  let req =
        newScan (db ^. appTable)
          & scan_filterExpression .~ Just "entityType = :entityType"
          & scan_expressionAttributeValues .~ Just (fromList [(":entityType", S entityType)])
  res <- AWS.runResourceT $ AWS.send (db ^. env) req
  case res ^. scanResponse_items of
    Nothing -> return []
    Just rs -> mapM DH.fromAttributeValueUnsafe rs

usersHandler :: Db -> IO [UserEntity]
usersHandler = list $ entityTypes ^. user

userHandler :: Db -> Id User -> IO (Maybe UserEntity)
userHandler = getById $ entityTypes ^. user

authoritiesHandler :: Db -> IO [AuthorityEntity]
authoritiesHandler = list $ entityTypes ^. authority

authortyHandler :: Db -> Id Authority -> IO (Maybe AuthorityEntity)
authortyHandler = getById $ entityTypes ^. authority
