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
import WaiAppStatic.Storage.Embedded (mkSettings)

import Api
import Emb (mkEmbedded)
import Model

newtype FileContent = FileContent {unRaw :: Lazy.ByteString}

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML FileContent where
  mimeRender _ = unRaw

startApp :: IO ()
startApp = run 8082 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

type API =
  Get '[HTML] FileContent
    :<|> "public" :> Raw
    :<|> "static" :> Raw
    :<|> "api" :> "users" :> Get '[JSON] [User]

server :: Server API
server =
  indexHandler
    :<|> serveDirectoryFileServer "./public"
    :<|> serveDirectoryWith $(mkSettings mkEmbedded)
    :<|> liftIO usersHandler

indexHandler :: Handler FileContent
indexHandler = do
  cnt <- liftIO $ Lazy.readFile "./public/index.html"
  return $ FileContent cnt
