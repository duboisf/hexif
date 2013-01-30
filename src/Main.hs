{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Exception (handle, SomeException)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy as BSL
import Data.Exif (parseExif)
import Data.Exif.Types
import Data.Char (toUpper)
import Numeric (showHex)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs, getProgName)
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

printUsage :: IO ()
printUsage = do
  prog <- getProgName
  putStrLn $ "usage: " ++ prog ++ " DIR"

main :: IO ()
main = do
  args <- getArgs
  if length args > 0
    then do
      srcDir <- head `liftM` getArgs
      exists <- doesDirectoryExist srcDir
      if exists
        then doWork srcDir
        else putStrLn (srcDir ++ " isn't a folder") >> exitFailure
    else printUsage

  where
    doWork :: String -> IO ()
    doWork srcDir = do
      let appConfig = AppConfig (Dir srcDir) (Dir ".")
      files <- runApp getFiles appConfig
      contents <- BSL.readFile $ let File file = head files in file
      let result = parseExif contents
      case result of
        (Left err, logs) -> print err >> print logs >> exitFailure
        (Right (header, fields, endPos), _) -> do
          print header
          forM_ (zip [1..] fields) $ \(n, field) -> do
            printLine
            putStrLn $ "Field " ++ show n
            printField field
          printLine
          putStrLn $ "End position: " ++ show endPos

    printLine = putStrLn $ replicate 20 '-'

    printField :: RawIFDField -> IO ()
    printField field =
      putStrLn $ 
        "Tag: " ++ show (rifTag field) ++
        "\nType: " ++ show (rifType field) ++
        "\nCount: " ++ show (rifCount field) ++
        "\nOffset: 0x" ++ map toUpper (showHex (rifOffset field) "")
