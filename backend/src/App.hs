{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Api
import Api (Db)
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
[Embedded.genStaticFileHandler||]
[Embedded.genLoginCallbackHandler||]
[Embedded.genLogoutCallbackHandler||]

startApp :: IO ()
startApp = do
  config <- Config.loadConfigWithDefault
  db <- Api.getDb config
  let portNum = Config.getPort $ config ^. port
  putStrLn $ "Listening on port " ++ show portNum
  run portNum $ app db

app :: Db -> Application
app db = serve api $ server db

api :: Proxy API
api = Proxy

type API =
  ( Get '[HTML] FileContent
      :<|> "signin-callback" :> Get '[HTML] FileContent
      :<|> "signout-callback" :> Get '[HTML] FileContent
      :<|> "public" :> Raw
  )
    :<|> "api"
      :> ( "authentication" :> "status" :> HeaderAuth :> Get '[JSON] Bool
            :<|> "users" :> Get '[JSON] [UserEntity]
            -- :<|> "users" :> HeaderAuth :> Capture "userId" Text :> Get '[JSON] (Maybe UserEntity)
            :<|> "authorities" :> Get '[JSON] [AuthorityEntity]
            :<|> "authorities" :> Capture "authorityId" Text :> Get '[JSON] (Maybe AuthorityEntity)
         )

type HeaderAuth = Header "Authorization" Text

server :: Db -> Server API
server db =
  ( indexHandler
      :<|> signinCallbackHandler
      :<|> signoutCallbackHandler
      :<|> staticFileHandler
      --  ( if config ^. runtimeEnv == Dev
      --         then serveDirectoryWebApp "public"
      --         else serveDirectoryWith $(mkSettings Embedded.staticFiles) -- index.html以外の静的ファイルもバイナリに埋め込む
      --      )
  )
    :<|> ( ( \auth -> do
              res <- liftIO $ Api.checkAuthentication auth
              if res
                then return True
                else throwError $ err401 {errBody = "Unauthorized"}
           )
            :<|> liftIO (Api.usersHandler db)
            -- :<|> ( \auth idValue ->
            --         liftIO (Api.userHandler db auth (Id idValue))
            --      )
            :<|> liftIO (Api.authoritiesHandler db)
            :<|> liftIO1 (Api.authortyHandler db . Id)
         )

liftIO1 :: (MonadIO m) => (a -> IO b) -> a -> m b
liftIO1 f x = liftIO (f x)
