{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Embedded (
  staticFiles,
  genIndexHandler,
  makeHandlerFromHtml,
  HTML (..),
  FileContent (..),
) where

import Crypto.Hash.MD5 (hashlazy)
import Network.Mime (defaultMimeLookup)

import System.Directory (getDirectoryContents)
import System.FilePath (pathSeparator)
import WaiAppStatic.Storage.Embedded

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class
import Data.ByteString.Lazy as Lazy (ByteString, readFile)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Network.HTTP.Media ((//), (/:))
import Servant

import Config

newtype FileContent = FileContent {unRaw :: ByteString}

data HTML = HTML
instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
instance MimeRender HTML FileContent where
  mimeRender _ = unRaw

genIndexHandler :: QuasiQuoter
genIndexHandler =
  QuasiQuoter
    { quoteExp = undefined :: String -> Q Exp
    , quotePat = undefined :: String -> Q Pat
    , quoteType = undefined :: String -> Q Type
    , quoteDec = genIndexHandlerCore :: String -> Q [Dec]
    }

genIndexHandlerCore :: FilePath -> Q [Dec]
genIndexHandlerCore pathWithSpace = do
  let name = mkName "indexHandler"
  let path = strip pathWithSpace
  config <- liftIO $ Config.loadConfigWithDefault
  if Config.runtimeEnv config == Dev
    then do
      -- 開発中は毎回ファイルを読み込む
      return
        [ SigD name (AppT (ConT ''Handler) (ConT ''FileContent)) -- 関数宣言部
        , FunD
            name
            [ Clause
                []
                ( NormalB $ AppE (VarE 'dynamicHandler) (LitE $ StringL path)
                )
                [] -- 実装定義部
            ]
        ]
    else do
      -- 開発以外ではコンパイル時にファイルを埋め込む
      html <- liftIO $ Prelude.readFile path
      return
        [ SigD name (AppT (ConT ''Handler) (ConT ''FileContent)) -- 関数宣言部
        , FunD
            name
            [ Clause
                []
                ( NormalB $ AppE (VarE 'makeHandlerFromHtml) (LitE $ StringL html)
                )
                [] -- 実装定義部
            ]
        ]

makeHandlerFromHtml :: ByteString -> Handler FileContent
makeHandlerFromHtml = return . FileContent

dynamicHandler :: FilePath -> Handler FileContent
dynamicHandler path = do
  html <- liftIO $ Lazy.readFile $ strip path
  return $ FileContent $ html

-- indexHandler :: Handler FileContent
-- indexHandler = do
--   cnt <- liftIO $ Lazy.readFile "./public/index.html"
--   return $ FileContent cnt

staticFiles :: IO [EmbeddableEntry]
staticFiles =
  listStaticFileNames
    >>= mapM fileToEmb

listStaticFileNames :: IO [FilePath]
listStaticFileNames =
  getDirectoryContents "public"
    >>= return . filter (\x -> x /= "." && x /= ".." && x /= "index.html")

fileToEmb :: FilePath -> IO EmbeddableEntry
fileToEmb fileName = do
  let path = "public" ++ [pathSeparator] ++ fileName
  cnt <- BL.readFile path
  return
    EmbeddableEntry
      { eLocation = T.pack fileName
      , eMimeType = defaultMimeLookup $ T.pack path
      , eContent = Left (hash cnt, cnt)
      }

hash :: BL.ByteString -> T.Text
hash = T.take 8 . T.decodeUtf8 . B64.encode . hashlazy

lstrip :: String -> String
lstrip [] = []
lstrip xs'@(x : xs)
  | x == ' ' = lstrip xs
  | otherwise = xs'

rstrip :: String -> String
rstrip = reverse . lstrip . reverse

strip :: String -> String
strip = lstrip . rstrip
