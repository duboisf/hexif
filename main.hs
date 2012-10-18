{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad (liftM)
import Control.Monad.Reader
import System.Directory (getDirectoryContents)

data AppConfig = AppConfig {
    acSrcDir :: FilePath
  , acTargetDir :: FilePath
  }

newtype App a = App {
    runA :: ReaderT AppConfig IO a
  } deriving (Monad, MonadIO, MonadReader AppConfig)

runApp :: App a -> AppConfig -> IO a
runApp app config = runReaderT (runA app) config

-- get list of files from source folder
getFiles :: App [FilePath]
getFiles = do
  srcDir <- acSrcDir `liftM` ask
  rawEntries <- liftIO $ getDirectoryContents srcDir
  let entries = filter (not . flip elem [".", ".."]) rawEntries
  return []

-- for each file f:
--   extract modification date modDate
--   build destFolder from modDate (targetDir/year/month)
--   if not exists destFolder, create it
--   copy f to destFolder

main :: IO ()
main = putStrLn "Hullo, world"
