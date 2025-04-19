{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module App (
  startApp,
  app,
) where

import Control.Lens
import Control.Monad.IO.Class
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import WaiAppStatic.Storage.Embedded (mkSettings)

import Api
import Config
import Embedded
import Entity
import Entity.Authority
import Entity.User

{-
index.htmlをバイナリに埋め込むための工夫
serveDirectoryWith $(mkSettings Embedded.staticFiles)
を使えればよかったのだが、ルーティングの関係でどうしてもうまくいかなかったので
index.htmlだけ別途埋め込むことにした.
やってることは、コンパイル時にindex.htmlを読み込んで
その内容をリテラルとして関数を呼び出すコードを生成している.
-}
[Embedded.genIndexHandler||]
[Embedded.genLoginCallbackHandler||]
[Embedded.genLogoutCallbackHandler||]

startApp :: IO ()
startApp = do
  config <- Config.loadConfigWithDefault
  db <- Api.getDb config
  let portNum = Config.getPort $ config ^. port
  putStrLn $ "Listening on port " ++ show portNum
  run portNum $ app config db

app :: Config -> Db -> Application
app config db = serve api $ server config db

api :: Proxy API
api = Proxy

type API =
  Get '[HTML] FileContent
    :<|> "signin-callback" :> Get '[HTML] FileContent
    :<|> "signout-callback" :> Get '[HTML] FileContent
    :<|> "public" :> Raw
    :<|> "api" :> "users" :> Get '[JSON] [UserEntity]
    :<|> "api" :> "users" :> Capture "userId" Text :> Get '[JSON] (Maybe UserEntity)
    :<|> "api" :> "authorities" :> Get '[JSON] [AuthorityEntity]
    :<|> "api" :> "authorities" :> Capture "authorityId" Text :> Get '[JSON] (Maybe AuthorityEntity)

server :: Config -> Db -> Server API
server config db =
  indexHandler
    :<|> signinCallbackHandler
    :<|> signoutCallbackHandler
    :<|> ( if config ^. runtimeEnv == Dev
            then serveDirectoryWebApp "public"
            else serveDirectoryWith $(mkSettings Embedded.staticFiles) -- index.html以外の静的ファイルもバイナリに埋め込む
         )
    :<|> liftIO (usersHandler db)
    :<|> liftIO1 (\idValue -> userHandler db (Id idValue))
    :<|> liftIO (authoritiesHandler db)
    :<|> liftIO1 (\idValue -> authortyHandler db (Id idValue))

liftIO1 :: (MonadIO m) => (a -> IO b) -> a -> m b
liftIO1 f x = liftIO (f x)
