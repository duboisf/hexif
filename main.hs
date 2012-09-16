{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Reader

data AppConfig = AppConfig {
    acSrcDir :: FilePath
  , acTargetDir :: FilePath
  }

newtype App a = App {
    runA :: ReaderT AppConfig IO a
  } deriving (Monad, MonadIO, MonadReader AppConfig)

-- get list of files from source folder
getFiles :: App FilePath
getFiles = undefined

-- for each file f:
--   extract modification date modDate
--   build destFolder from modDate (targetDir/year/month)
--   if not exists destFolder, create it
--   copy f to destFolder

main :: IO ()
main = putStrLn "Hullo, world"
