{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api (
  Db,
  getDb,
  usersHandler,
  authoritiesHandler,
) where

import Amazonka as AWS
import Amazonka.DynamoDB.Scan
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.Prelude (HashMap)
import Config
import Control.Exception.Safe (throwString)
import Control.Lens
import Control.Lens.TH
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar
import System.Environment
import System.IO (stdout)

import Model

data TableNames = TableNames
  { _tableNamesAuthority :: Text
  , _tableNamesUser :: Text
  }
  deriving (Show, Eq, Read)
makeFields ''TableNames

data Db = Db Env TableNames

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
authoritiesHandler (Db env tableNames) = do
  res <- AWS.runResourceT $ AWS.send env $ newScan (tableNames ^. authority)
  case res ^. scanResponse_items of
    Nothing -> return []
    Just rs -> mapM recToAuthority rs
 where
  recToAuthority :: HashMap Text AttributeValue -> IO Authority
  recToAuthority rec = do
    ca <- getString rec "createdAt"
    ua <- getString rec "updatedAt"
    lv <- getInt rec "level"
    nm <- getString rec "name"
    return $ Authority (Text.unpack ca) (Text.unpack ua) lv (Text.unpack nm)

getString :: HashMap Text AttributeValue -> Text -> IO Text
getString values propertyName =
  case HashMap.lookup propertyName values of
    Just (S s) -> return s
    _ -> throwString ("property '" ++ (Text.unpack propertyName) ++ "' notfound.")

getInt :: HashMap Text AttributeValue -> Text -> IO Int
getInt values propertyName =
  case HashMap.lookup propertyName values of
    Just (N s) -> return $ read $ Text.unpack s
    _ -> throwString ("property '" ++ (Text.unpack propertyName) ++ "' notfound.")

usersHandler :: Db -> IO [User]
usersHandler _ =
  return
    [ User 1 "Isaac, " "Newton" (fromGregorian 1683 3 1)
    , User 2 "Albert, " "Einstein" (fromGregorian 1905 12 1)
    , User 3 "jabara, " "star" (fromGregorian 1975 9 29)
    , User 4 "Rakuda, " "amou" (fromGregorian 2010 3 3)
    ]
