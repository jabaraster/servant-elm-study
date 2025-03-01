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

import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import WaiAppStatic.Storage.Embedded (mkSettings)

import Api
import Config
import Embedded
import Model

{-
index.htmlをバイナリに埋め込むための工夫
serveDirectoryWith $(mkSettings Embedded.staticFiles)
を使えればよかったのだが、ルーティングの関係でどうしてもうまくいかなかったので
index.htmlだけ別途埋め込むことにした.
やってることは、コンパイル時にindex.htmlを読み込んで
その内容をリテラルとして関数を呼び出すコードを生成している.
-}
[Embedded.genIndexHandler| ./public/index.html |]

startApp :: IO ()
startApp = do
  config <- Config.loadConfigWithDefault
  let portNum = Config.getPort $ Config.port config
  putStrLn $ "Listening on port " ++ show portNum
  run portNum $ app config

app :: Config -> Application
app config = serve api $ server config

api :: Proxy API
api = Proxy

type API =
  Get '[HTML] FileContent
    :<|> "public" :> Raw
    :<|> "api" :> "users" :> Get '[JSON] [User]
    :<|> "api" :> "authorities" :> Get '[JSON] [Authority]

server :: Config -> Server API
server config =
  indexHandler
    :<|> ( if Config.runtimeEnv config == Dev
            then serveDirectoryWebApp "public"
            else serveDirectoryWith $(mkSettings Embedded.staticFiles) -- index.html以外の静的ファイルもバイナリに埋め込む
         )
    :<|> liftIO usersHandler
    :<|> liftIO authoritiesHandler
