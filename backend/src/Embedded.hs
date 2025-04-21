{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Crypto.Hash.MD5 (hashlazy)
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Data.List (isSuffixOf)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.HTTP.Media ((//), (/:))
import Network.Mime (defaultMimeLookup)
import Servant
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import WaiAppStatic.Storage.Embedded

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Config

newtype FileContent = FileContent {unRaw :: ByteString}

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML FileContent where
  mimeRender _ = unRaw

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
  config <- liftIO $ Config.loadConfigWithDefault
  if (config ^. runtimeEnv) == Dev
    then genStaticFileHandlerDynamic directoryPath functionName
    else genStaticFileHandlerEmbedded directoryPath functionName

genStaticFileHandlerDynamic :: String -> Name -> Q [Dec]
genStaticFileHandlerDynamic directoryPath functionName =
  return
    [ FunD
        functionName
        [ Clause
            []
            ( NormalB $ AppE (VarE 'serveDirectoryWebApp) (LitE $ StringL directoryPath)
            )
            [] -- 実装定義部
        ]
    ]

genStaticFileHandlerEmbedded :: String -> Name -> Q [Dec]
genStaticFileHandlerEmbedded directoryPath functionName = do
  files <-
    liftIO
      ( getDirectoryContents directoryPath
          >>= return
            . filter
              ( \fileName ->
                  fileName /= "."
                    && fileName /= ".."
                    && (not $ isSuffixOf ".html" fileName)
              )
      )
  embeddedFiles <- liftIO $ forM files $ \fileName -> do
    content <- Prelude.readFile (directoryPath </> fileName)
    return (fileName, content)
  let tupleExps = map (\(fp, bs) -> TupE [Just $ LitE $ StringL fp, Just $ LitE $ StringL bs]) embeddedFiles
  let embeddedFilesListExp = ListE tupleExps
  let appExp = AppE (VarE 'serveDirectoryEmbedded) embeddedFilesListExp
  let functionDec = FunD functionName [Clause [] (NormalB appExp) []]
  return [{-SigD functionName functionType,-} functionDec]

lstrip :: String -> String
lstrip [] = []
lstrip xs'@(x : xs)
  | x == ' ' = lstrip xs
  | otherwise = xs'

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip
