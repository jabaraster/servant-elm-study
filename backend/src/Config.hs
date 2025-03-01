module Config (
  Config (..),
  Port,
  RuntimeEnv (..),
  getPort,
  loadConfig,
  loadConfigWithDefault,
  default_,
  defaultPort,
  defaultRuntimeEnv,
) where

import System.Environment
import Text.Read (readMaybe)

data Port = Port Int
  deriving (Show, Eq, Read)

data RuntimeEnv = Dev | Prod
  deriving (Show, Eq, Read)

tryPort :: Int -> Maybe Port
tryPort num =
  if num < 0
    then Nothing
    else
      if num > 65535
        then Nothing
        else Just $ Port num

getPort :: Port -> Int
getPort (Port num) = num

data Config = Config
  { port :: Port
  , runtimeEnv :: RuntimeEnv
  }
  deriving (Show, Eq, Read)

defaultPort :: Port
defaultPort = Port 8080

defaultRuntimeEnv :: RuntimeEnv
defaultRuntimeEnv = Dev

default_ :: Config
default_ = Config defaultPort defaultRuntimeEnv

loadConfig :: IO (Either [String] Config)
loadConfig = do
  ePort <- readPortFromEnv
  eRuntimeEnv <- readRuntimeEnvFromEnv
  return $ case (ePort, eRuntimeEnv) of
    (Right port, Right runtimeEnv) -> Right $ Config port runtimeEnv
    (Left portErr, Left runtimeEnvErr) -> Left [portErr, runtimeEnvErr]
    (Left portErr, _) -> Left [portErr]
    (_, Left runtimeEnvErr) -> Left [runtimeEnvErr]

loadConfigWithDefault :: IO Config
loadConfigWithDefault = do
  eConfig <- Config.loadConfig
  case eConfig of
    Right config -> do
      putStrLn $ "Loaded config: " ++ show config
      return config
    Left errs -> do
      mapM_ putStrLn errs
      putStrLn "デフォルト設定を採用します..."
      return Config.default_

readPortFromEnv :: IO (Either String Port)
readPortFromEnv = lookupEnv "PORT" >>= parsePort
 where
  parsePort :: Maybe String -> IO (Either String Port)
  parsePort Nothing = do
    putStrLn $ "PORT is not set. Accept default value. [" ++ (show $ getPort defaultPort) ++ "]"
    return $ Right defaultPort
  parsePort (Just portS) =
    return $ case readMaybe portS of
      Nothing -> Left $ "PORT is not a valid port number. [" ++ portS ++ "]"
      Just portI ->
        case tryPort portI of
          Nothing -> Left $ "PORT is not a valid port number. [" ++ portS ++ "]"
          Just port -> Right port

readRuntimeEnvFromEnv :: IO (Either String RuntimeEnv)
readRuntimeEnvFromEnv = lookupEnv "RUNTIME_ENV" >>= parseRuntimeEnv
 where
  parseRuntimeEnv :: Maybe String -> IO (Either String RuntimeEnv)
  parseRuntimeEnv Nothing = do
    putStrLn $ "RUNTIME_ENV is not set. Accept default value. [" ++ (show defaultRuntimeEnv) ++ "]"
    return $ Right defaultRuntimeEnv
  parseRuntimeEnv (Just envS) =
    return $ case readMaybe envS of
      Nothing -> Left $ "RUNTIME_ENV is not a valid runtime environment. [" ++ envS ++ "]"
      Just env -> Right env
