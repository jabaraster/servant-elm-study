{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
| * 静的ファイルのハンドラをソースコード中に埋め込むための関数群を提供する.
|
-}
module Embedded (
  genIndexHandler,
  genLoginCallbackHandler,
  genLogoutCallbackHandler,
  genStaticFileHandler,
  genShortHtmlHandler,
  genHtmlHandlerEmbedded,
  HTML (..),
  FileContent (..),
) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.FileEmbed
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (lift)
import Network.HTTP.Media ((//), (/:))
import Servant
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import Config

{- |
| ファイル内容を返すための定義
|
-}
newtype FileContent = FileContent {unRaw :: Lazy.ByteString}

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML FileContent where
  mimeRender _ = unRaw

{- |
| HTMLファイルを返すための仕組み.
| RUNTIME_ENV=Devの場合はリクエストがある度にファイルの内容を読み込んで返す.
| RUNTIME_ENV=Prodの場合はソースコード中にHTMLファイルの内容を埋め込む.
|
-}
genHtmlHandler :: String -> FilePath -> String -> Q [Dec]
genHtmlHandler functionName pathWithSpace _ = do
  let path = strip pathWithSpace
  config <- liftIO $ Config.loadConfigWithDefault
  if (config ^. runtimeEnv) == Dev
    then genHtmlHandlerDynamic functionName path
    else genHtmlHandlerEmbedded functionName path

htmlHandlerSigniture :: String -> Dec
htmlHandlerSigniture functionName = SigD (mkName functionName) (AppT (ConT ''Handler) (ConT ''FileContent))

{- | dynami load html file.

> indexHtmlHandler :: Handler FileContent
> indexHtmlHandler = htmlHandlerDynamic "static/index.html"
-}
genHtmlHandlerDynamic :: String -> FilePath -> Q [Dec]
genHtmlHandlerDynamic functionName htmlPath = do
  return
    [ htmlHandlerSigniture functionName
    , FunD
        (mkName functionName)
        [ Clause
            []
            ( NormalB $ AppE (VarE 'htmlHandlerDynamic) (LitE $ StringL htmlPath)
            )
            []
        ]
    ]

embedTextContent :: FilePath -> Q Exp
embedTextContent filePath =
  liftIO $ Lazy.readFile filePath >>= lift

{- | embedded html content.
>
> indexHtmlHandler :: Handler FileContent
> indexHtmlHandler = htmlHandlerEmbedded "<htmlファイル内容のLazy.ByteString表現>"
-}
genHtmlHandlerEmbedded :: String -> FilePath -> Q [Dec]
genHtmlHandlerEmbedded functionName htmlPath = do
  htmlExp <- embedTextContent htmlPath
  return
    [ htmlHandlerSigniture functionName
    , FunD
        (mkName functionName)
        [ Clause
            []
            ( NormalB $ AppE (VarE 'htmlHandlerEmbedded) htmlExp
            )
            []
        ]
    ]

htmlHandlerDynamic :: FilePath -> Handler FileContent
htmlHandlerDynamic path = do
  html <- liftIO $ Lazy.readFile $ strip path
  return $ FileContent $ html

htmlHandlerEmbedded :: ByteString -> Handler FileContent
htmlHandlerEmbedded = return . FileContent

{- |
| HTMLページを返すハンドラを生成する関数群.
|
-}
genShortHtmlHandler :: QuasiQuoter
genShortHtmlHandler =
  QuasiQuoter
    { quoteExp = undefined :: String -> Q Exp
    , quotePat = undefined :: String -> Q Pat
    , quoteType = undefined :: String -> Q Type
    , quoteDec = genHtmlHandler "shortHtmlHandler" "./public/short.html" :: String -> Q [Dec]
    }

genIndexHandler :: QuasiQuoter
genIndexHandler =
  QuasiQuoter
    { quoteExp = undefined :: String -> Q Exp
    , quotePat = undefined :: String -> Q Pat
    , quoteType = undefined :: String -> Q Type
    , quoteDec = genHtmlHandler "indexHandler" "./public/index.html" :: String -> Q [Dec]
    }

genLoginCallbackHandler :: QuasiQuoter
genLoginCallbackHandler =
  QuasiQuoter
    { quoteExp = undefined :: String -> Q Exp
    , quotePat = undefined :: String -> Q Pat
    , quoteType = undefined :: String -> Q Type
    , quoteDec = genHtmlHandler "signinCallbackHandler" "./public/signin-callback.html" :: String -> Q [Dec]
    }

genLogoutCallbackHandler :: QuasiQuoter
genLogoutCallbackHandler =
  QuasiQuoter
    { quoteExp = undefined :: String -> Q Exp
    , quotePat = undefined :: String -> Q Pat
    , quoteType = undefined :: String -> Q Type
    , quoteDec = genHtmlHandler "signoutCallbackHandler" "./public/signout-callback.html" :: String -> Q [Dec]
    }

{- |
| HTML以外の静的ファイルを返すハンドラを生成する関数群.
|
-}
genStaticFileHandler :: QuasiQuoter
genStaticFileHandler =
  QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec = \_ -> genStaticFileHandlerCore
    }

genStaticFileHandlerCore :: Q [Dec]
genStaticFileHandlerCore = do
  let directoryPath = "public"
  let functionName = mkName "staticFileHandler"
  let signiture =
        SigD
          (mkName "staticFileHandler")
          (AppT (AppT (ConT ''ServerT) (ConT ''Raw)) (VarT $ mkName "m")) -- ServerT Raw m
  config <- liftIO $ Config.loadConfigWithDefault
  if (config ^. runtimeEnv) == Dev
    then genStaticFileHandlerDynamic signiture directoryPath functionName
    else genStaticFileHandlerEmbedded signiture directoryPath functionName

{- | dynamic loading static file.

> staticFileHandler :: ServerT Raw m
> staticFileHandler = sreveDirectoryWebApp "static"
-}
genStaticFileHandlerDynamic :: Dec -> String -> Name -> Q [Dec]
genStaticFileHandlerDynamic signiture directoryPath functionName =
  return
    [ signiture
    , FunD
        functionName
        [ Clause
            []
            ( NormalB $ AppE (VarE 'serveDirectoryWebApp) (LitE $ StringL directoryPath)
            )
            []
        ]
    ]

{- | embedded statci file content.

> staticFileHandler :: ServerT Raw m
> staticFileHandler = serveDirectoryEmbedded [
>     ("app.js", "<ByteString literal>")
>   , ("app.css", "<ByteString literal>")
>   ]
-}
genStaticFileHandlerEmbedded :: Dec -> String -> Name -> Q [Dec]
genStaticFileHandlerEmbedded signiture directoryPath functionName = do
  files <- liftIO $ listStaticFileNames directoryPath
  tupleExps <-
    mapM
      ( \fileName -> do
          bsExp <- Data.FileEmbed.embedFile (directoryPath </> fileName)
          return $ TupE [Just $ LitE $ StringL fileName, Just bsExp]
      )
      files
  let embeddedFilesListExp = ListE tupleExps
  let appExp = AppE (VarE 'serveDirectoryEmbedded) embeddedFilesListExp
  let functionDec = FunD functionName [Clause [] (NormalB appExp) []]
  return [signiture, functionDec]

listStaticFileNames :: FilePath -> IO [String]
listStaticFileNames directoryPath =
  getDirectoryContents directoryPath
    >>= return
      . filter
        ( \fileName ->
            fileName /= "."
              && fileName /= ".."
        )

{- |
| 低次元のヘルパー関数
|
-}
lstrip :: String -> String
lstrip [] = []
lstrip xs'@(x : xs)
  | x == ' ' = lstrip xs
  | otherwise = xs'

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip
