module QHelper (
  embedTextContent,
) where

import Control.Monad.IO.Class (liftIO)
import Data.FileEmbed
import Data.List (isSuffixOf)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)
import System.IO.Error (catchIOError, isDoesNotExistError)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

embedTextContent :: FilePath -> Q Exp
embedTextContent filePath = do
  liftIO $ putStrLn "-----------------------"
  liftIO $ putStrLn $ "Processing [" ++ filePath ++ "]"
  liftIO $ putStrLn "-----------------------"
  -- Data.FileEmbed.embedFile filePath

  content <- liftIO $ readFileUTF8String filePath
  liftIO $
    if ".html" `isSuffixOf` filePath
      then putStrLn content
      else putStr ""
  lift content

readFileUTF8String :: FilePath -> IO String
readFileUTF8String filePath =
  catchIOError (readFileInternal filePath) $ \e ->
    if isDoesNotExistError e
      then ioError $ userError $ "ファイルが存在しません: " ++ filePath
      else ioError e

readFileInternal :: FilePath -> IO String
readFileInternal filePath = TIO.readFile filePath >>= return . T.unpack
