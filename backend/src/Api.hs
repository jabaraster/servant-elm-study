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
  authortyByNameHandler,
) where

import Amazonka as AWS
import Amazonka.DynamoDB.GetItem
import Amazonka.DynamoDB.Query
import Amazonka.DynamoDB.Scan
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.Prelude (fromList)
import Config
import Control.Exception.Safe (throwString)
import Control.Lens
import Control.Lens.TH
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock
import qualified Jabara.Amazonka.DynamoDB.Helper as DH
import System.Environment
import System.IO (stdout)

import Model

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

authoritiesHandler :: Db -> IO [Authority]
authoritiesHandler db = do
  res <- AWS.runResourceT $ AWS.send (db ^. env) $ newScan (db ^. tableNames ^. authority)
  case res ^. scanResponse_items of
    Nothing -> return []
    Just rs -> mapM DH.fromAttributeValueUnsafe rs

usersHandler :: Db -> IO [User]
usersHandler db = do
  res <- AWS.runResourceT $ AWS.send (db ^. env) $ newScan (db ^. tableNames ^. user)
  case res ^. scanResponse_items of
    Nothing -> return []
    Just rs -> mapM DH.fromAttributeValueUnsafe rs

userHandler :: Db -> Integer -> IO (Maybe User)
userHandler db idValue = do
  let req =
        newGetItem (db ^. tableNames ^. user)
          & getItem_key .~ fromList [("id", N $ Text.pack $ show idValue)]
  res <- AWS.runResourceT $ AWS.send (db ^. env) req
  case res ^. getItemResponse_item of
    Nothing -> return Nothing
    Just rec -> do
      user <- DH.fromAttributeValueUnsafe rec
      return $ Just user

authortyByNameHandler :: Db -> Text -> IO (Maybe Authority)
authortyByNameHandler db name = do
  let req =
        newQuery (db ^. tableNames ^. authority)
          & query_keyConditionExpression .~ Just "#n = :name"
          & query_expressionAttributeNames .~ (Just $ fromList [("#n", "name")])
          & query_expressionAttributeValues .~ (Just $ fromList [(":name", S name)])
          & query_limit .~ Just 1
  res <- AWS.runResourceT $ AWS.send (db ^. env) req
  case res ^. queryResponse_items of
    [] -> return Nothing
    [rec] -> do
      auth <- DH.fromAttributeValueUnsafe rec
      return $ Just auth
    _ -> throwString "multiple authority found."
