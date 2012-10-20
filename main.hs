{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Exception (handle, SomeException)
import Control.Monad (liftM)
import Control.Monad.Reader
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))

data File = File FilePath deriving Show

data Dir = Dir FilePath deriving Show

data AppConfig = AppConfig {
    acSrcDir :: Dir
  , acTargetDir :: Dir
  }

newtype App a = App {
    runA :: ReaderT AppConfig IO a
  } deriving (Monad, MonadIO, MonadReader AppConfig)

runApp :: App a -> AppConfig -> IO a
runApp app config = runReaderT (runA app) config

-- get list of files from source folder
getFiles :: App [File]
getFiles = do
  srcDir <- acSrcDir `liftM` ask
  liftIO $ getAllFilesRecursively srcDir

filterDirEntries :: [FilePath] -> [FilePath]
filterDirEntries = filter (not . flip elem [".", ".."])

nothingHandler :: SomeException -> IO (Maybe a)
nothingHandler _ = return Nothing

safeGetDirContents :: Dir -> IO (Maybe [FilePath])
safeGetDirContents (Dir dir) = handle nothingHandler $
  Just `liftM` getDirectoryContents dir

getFilteredDirContents :: Dir -> IO [FilePath]
getFilteredDirContents (Dir dir) = do
  maybeEntries <- safeGetDirContents $ Dir dir
  case maybeEntries of
    Nothing -> return []
    Just files -> return $ map (dir </>) $ filterDirEntries files
  
partitionFileEntries :: [FilePath] -> IO ([Dir], [File])
partitionFileEntries entries =
  part entries [] []
  where
    part :: [FilePath] -> [Dir] -> [File] -> IO ([Dir], [File])
    part [] dirs files = return (dirs, files)
    part (entry:entries) dirs files = do
      isDir <- doesDirectoryExist entry
      if isDir
        then part entries (Dir entry : dirs) files
        else do
          isFile <- doesFileExist entry 
          if isFile
            then part entries dirs (File entry : files)
            else part entries dirs files

getPartitionedDirContents :: Dir -> IO ([Dir], [File])
getPartitionedDirContents dir =
  getFilteredDirContents dir >>= partitionFileEntries

getAllFilesRecursively :: Dir -> IO [File]
getAllFilesRecursively dir =
  getFiles' [dir] []
  where
    getFiles' :: [Dir] -> [File] -> IO [File]
    getFiles' [] files = return files
    getFiles' (dir:dirs) files = do
      (newDirs, newFiles) <- getPartitionedDirContents dir
      getFiles' (dirs ++ newDirs) (files ++ newFiles)
  
-- for each file f:
--   extract modification date modDate
--   build destFolder from modDate (targetDir/year/month)
--   if not exists destFolder, create it
--   copy f to destFolder

main :: IO ()
main = do
  srcDir <- head `liftM` getArgs
  exists <- doesDirectoryExist srcDir
  case exists of
    True -> do
      let appConfig = AppConfig (Dir srcDir) (Dir ".")
      runApp getFiles appConfig >>= mapM_ print
--    False -> exitFailure
