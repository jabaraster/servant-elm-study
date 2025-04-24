{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module App (startApp) where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api
import Config
import Embedded
import Entity
import Entity.Authority
import Entity.User

startApp :: IO ()
startApp = do
  config <- Config.loadConfigWithDefault
  db <- Api.getDb config
  let portNum = Config.getPort $ config ^. port
  putStrLn "----------------------------------------------"
  putStrLn "- start 'servant-elm-study' server"
  putStrLn $ "- Listening port : " ++ show portNum
  putStrLn $ "- Runtime env    : " ++ (show (config ^. runtimeEnv))
  putStrLn "----------------------------------------------"
  run portNum $ app db

app :: Db -> Application
app db = serve api $ server db

api :: Proxy API
api = Proxy

type API =
  ( Get '[HTML] FileContent
      :<|> "signin-callback" :> Get '[HTML] FileContent
      :<|> "signout-callback" :> Get '[HTML] FileContent
      :<|> "short" :> Get '[HTML] FileContent
      :<|> "public" :> Raw
  )
    :<|> "api"
      :> ( "authentication" :> "status" :> HeaderAuth :> Get '[JSON] Bool
            :<|> "users" :> Get '[JSON] [UserEntity]
            :<|> "authorities" :> Get '[JSON] [AuthorityEntity]
            :<|> "authorities" :> Capture "authorityId" Text :> Get '[JSON] (Maybe AuthorityEntity)
         )

type HeaderAuth = Header "Authorization" Text

server :: Db -> Server API
server db =
  ( $(Embedded.defHtmlHandler "public/index.html")
      :<|> $(Embedded.defHtmlHandler "public/signin-callback.html")
      :<|> $(Embedded.defHtmlHandler "public/signout-callback.html")
      :<|> $(Embedded.defHtmlHandler "public/short.html")
      :<|> $(Embedded.defStaticFileHandler "public")
  )
    :<|> ( ( \auth -> do
              res <- liftIO $ Api.checkAuthentication auth
              if res
                then return True
                else throwError $ err401 {errBody = "Unauthorized"}
           )
            :<|> liftIO (Api.usersHandler db)
            :<|> liftIO (Api.authoritiesHandler db)
            :<|> liftIO1 (Api.authortyHandler db . Id)
         )

liftIO1 :: (MonadIO m) => (a -> IO b) -> a -> m b
liftIO1 f x = liftIO (f x)
