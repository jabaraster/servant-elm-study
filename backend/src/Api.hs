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
import System.Environment
import System.IO (stdout)

import Entity

data TableNames = TableNames
  { _tableNamesAuthority :: Text
  , _tableNamesUser :: Text
  }
  deriving (Show, Eq, Read)
makeFields ''TableNames

data Db = Db
  { _dbEnv :: Env
  , _dbTableNames :: TableNames
  }
makeFields ''Db

getDb :: Config -> IO Db
getDb config = do
  logger <- AWS.newLogger (if config ^. runtimeEnv == Dev then AWS.Debug else AWS.Info) stdout
  discoveredEnv <- AWS.newEnv AWS.discover
  tableNames <- loadTableNames

  let env = discoveredEnv {AWS.logger = logger, AWS.region = AWS.Tokyo}
  return $ Db env tableNames

loadTableNames :: IO TableNames
loadTableNames = do
  envs <- getEnvironment
  let mAuth = Prelude.lookup "TABLE_NAME_AUTHORITY" envs
  let mUser = Prelude.lookup "TABLE_NAME_USER" envs
  case (mAuth, mUser) of
    (Just auth, Just user) ->
      return $ TableNames (Text.pack auth) (Text.pack user)
    _ ->
      throwString "table name not found."

getById :: (FromAttributeValue a) => Env -> Text -> Id b -> IO (Maybe a)
getById env tableName id_ = do
  let req =
        newGetItem tableName
          & getItem_key .~ fromList [("id", N $ Text.pack $ show $ id_ ^. value)]
  res <- AWS.runResourceT $ AWS.send env req
  case res ^. getItemResponse_item of
    Nothing -> return Nothing
    Just rec -> do
      ret <- DH.fromAttributeValueUnsafe rec
      return $ Just ret

list :: (FromAttributeValue a) => Env -> Text -> IO [a]
list env tableName = do
  res <- AWS.runResourceT $ AWS.send env $ newScan tableName
  case res ^. scanResponse_items of
    Nothing -> return []
    Just rs -> mapM DH.fromAttributeValueUnsafe rs

usersHandler :: Db -> IO [UserRecord]
usersHandler (Db env tableNames) = list env (tableNames ^. user)

userHandler :: Db -> Id User -> IO (Maybe UserRecord)
userHandler (Db env tableNames) = getById env (tableNames ^. user)

authoritiesHandler :: Db -> IO [AuthorityRecord]
authoritiesHandler (Db env tableNames) = list env (tableNames ^. authority)

authortyHandler :: Db -> Id Authority -> IO (Maybe AuthorityRecord)
authortyHandler (Db env tableNames) = getById env (tableNames ^. authority)
