{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Exception (handle, SomeException)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.Exif (parseExif)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitFailure)
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
runApp app = runReaderT (runA app)

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
          part entries dirs (if isFile then File entry : files else files)

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
  if exists
    then do
      let appConfig = AppConfig (Dir srcDir) (Dir ".")
      runApp getFiles appConfig -- >>= mapM_ print
      contents <- BSL.readFile "./IMG_2845.JPG"
      let result = parseExif contents
      case result of
        Left err -> print err >> exitFailure
        Right (header, fields, endPos) -> do
          putStrLn "Success"
          print header
          mapM_ print fields
          putStrLn $ "End position: " ++ show endPos
    else putStrLn (srcDir ++ " isn't a folder") >> exitFailure

