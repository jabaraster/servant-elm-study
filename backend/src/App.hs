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
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy as Lazy
import Data.Time.Calendar
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { id :: Int
  , firstName :: String
  , lastName :: String
  , registrationDate :: Day
  }
  deriving (Eq, Show)
$(deriveJSON defaultOptions ''User)

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
    :<|> serveDirectoryFileServer "./static"
    :<|> usersHandler

indexHandler :: Handler FileContent
indexHandler = do
  cnt <- liftIO $ Lazy.readFile "./static/index.html"
  return $ FileContent cnt

usersHandler :: Handler [User]
usersHandler =
  return
    [ User 1 "Isaac" "Newton" (fromGregorian 1683 3 1)
    , User 2 "Albert" "Einstein" (fromGregorian 1905 12 1)
    , User 3 "Tomoyuki" "Kawano" (fromGregorian 1975 9 29)
    ]
