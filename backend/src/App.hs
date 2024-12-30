{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module App (
  startApp,
  app,
) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api
import Model

newtype FileContent = FileContent {unRaw :: Lazy.ByteString}

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML FileContent where
  mimeRender _ = unRaw

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type API =
  Get '[HTML] FileContent
    :<|> "public" :> Raw
    :<|> "api" :> "users" :> Get '[JSON] [User]

server :: Server API
server =
  indexHandler
    :<|> serveDirectoryFileServer "./public"
    :<|> liftIO usersHandler

indexHandler :: Handler FileContent
indexHandler = do
  cnt <- liftIO $ Lazy.readFile "./public/index.html"
  return $ FileContent cnt