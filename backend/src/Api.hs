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
import qualified Jabara.Amazonka.DynamoDB.Helper as DH
import Amazonka.Prelude (HashMap)
import Config
import Control.Lens
import Control.Lens.TH
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Calendar
import Control.Exception.Safe (throwString)
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
    ca <- DH.getTextUnsafe rec "createdAt"
    ua <- DH.getTextUnsafe rec "updatedAt"
    lv <- DH.getIntegerUnsafe rec "level"
    nm <- DH.getTextUnsafe rec "name"
    return $ Authority ca ua lv nm

usersHandler :: Db -> IO [User]
usersHandler _ =
  return
    [ User 1 "Isaac, " "Newton" (fromGregorian 1683 3 1)
    , User 2 "Albert, " "Einstein" (fromGregorian 1905 12 1)
    , User 3 "jabara, " "star" (fromGregorian 1975 9 29)
    , User 4 "Rakuda, " "amou" (fromGregorian 2010 3 3)
    ]
