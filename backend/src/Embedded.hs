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
  HTML (..),
  FileContent (..),
) where

import Control.Lens
import Control.Monad (forM)
import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.List (isSuffixOf)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.HTTP.Media ((//), (/:))
import Servant
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))

import Config

{- |
| ファイル内容を返すための定義
|
-}
newtype FileContent = FileContent {unRaw :: ByteString}

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
  let name = mkName functionName
  let path = strip pathWithSpace
  let sig = SigD name (AppT (ConT ''Handler) (ConT ''FileContent))
  config <- liftIO $ Config.loadConfigWithDefault
  if (config ^. runtimeEnv) == Dev
    then -- 開発中は毎回ファイルを読み込む

      return
        [ sig
        , FunD
            name
            [ Clause
                []
                ( NormalB $ AppE (VarE 'genHtmlHandlerDynamic) (LitE $ StringL path)
                )
                [] -- 実装定義部
            ]
        ]
    else do
      -- 開発以外ではコンパイル時にファイルを埋め込む
      html <- liftIO $ Prelude.readFile path
      return
        [ sig
        , FunD
            name
            [ Clause
                []
                ( NormalB $ AppE (VarE 'genHtmlHandlerEmbedded) (LitE $ StringL html)
                )
                [] -- 実装定義部
            ]
        ]

genHtmlHandlerEmbedded :: ByteString -> Handler FileContent
genHtmlHandlerEmbedded = return . FileContent

genHtmlHandlerDynamic :: FilePath -> Handler FileContent
genHtmlHandlerDynamic path = do
  html <- liftIO $ Lazy.readFile $ strip path
  return $ FileContent $ html

{- |
| HTMLページを返すハンドラを生成する関数群.
|
-}
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
| 画像などのバイナリが埋め込めないのが辛い.
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

genStaticFileHandlerEmbedded :: Dec -> String -> Name -> Q [Dec]
genStaticFileHandlerEmbedded signiture directoryPath functionName = do
  files <- liftIO $ listStaticFileNames directoryPath
  embeddedFiles <- liftIO $ forM files $ \fileName -> do
    liftIO $ putStrLn $ "!!! file [" ++ fileName ++ "] embedding..."
    content <- Prelude.readFile (directoryPath </> fileName)
    return (fileName, content)
  let tupleExps = map (\(fp, bs) -> TupE [Just $ LitE $ StringL fp, Just $ LitE $ StringL bs]) embeddedFiles
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
              && ( (isSuffixOf ".js" fileName)
                    || (isSuffixOf ".css" fileName)
                    || (isSuffixOf ".map" fileName)
                 )
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
