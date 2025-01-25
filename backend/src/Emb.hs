{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Emb (mkEmbedded) where

import Crypto.Hash.MD5 (hashlazy)
import Network.Mime (defaultMimeLookup)

import System.Directory (getDirectoryContents)
import System.FilePath (pathSeparator)
import WaiAppStatic.Storage.Embedded

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

listStaticFileNames :: IO [FilePath]
listStaticFileNames =
  getDirectoryContents "public"
    >>= return . filter (\x -> x /= "." && x /= "..")

fileToEmb :: FilePath -> IO EmbeddableEntry
fileToEmb fileName = do
  let path = "public" ++ [pathSeparator] ++ fileName
  cnt <- BL.readFile path
  return
    EmbeddableEntry
      { eLocation = T.pack path
      , eMimeType = defaultMimeLookup $ T.pack path
      , eContent = Left (hash cnt, cnt)
      }

hash :: BL.ByteString -> T.Text
hash = T.take 8 . T.decodeUtf8 . B64.encode . hashlazy

mkEmbedded :: IO [EmbeddableEntry]
mkEmbedded =
  listStaticFileNames
    >>= mapM fileToEmb