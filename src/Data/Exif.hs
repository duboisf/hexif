{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif where

import Control.Monad (void)
import Control.Monad.Error (Error(..), ErrorT, lift, runErrorT, throwError)
import Control.Monad.Error.Class (MonadError)
import Data.Binary.Get (Get, getWord16be, getWord16le, runGet)
import Data.ByteString.Lazy (ByteString)
import Data.Word (Word16)
import Numeric (showHex)

data BParserError = Unsatisfied String
                  | BParserError
  deriving (Show)

instance Error BParserError where
  noMsg = BParserError
  strMsg = Unsatisfied

newtype BParser a = BP {
  runBP :: ErrorT BParserError Get a
} deriving (Functor, Monad, MonadError BParserError)

runBParser :: BParser a -> ByteString -> Either BParserError a
runBParser parser = runGet (runErrorT (runBP parser))

satisfy :: (Show a, Eq a, Integral a) => String -> a -> a -> BParser a
satisfy desc expected actual =
  if actual == expected
    then return actual
    else throwError (Unsatisfied errorMsg)
  where
    msgList = ["Expected", desc, expectedString, "got", showHex' actual]
    expectedString = "(" ++ showHex' expected ++ ")"
    errorMsg = unwords msgList
    showHex' :: (Integral a, Show a) => a -> String
    showHex' number = "0x" ++ showHex number ""

satisfy_ :: (Show a, Eq a, Integral a) => String -> a -> a -> BParser ()
satisfy_ desc expected actual =
  void $ satisfy desc expected actual

liftP :: Get a -> BParser a
liftP m = BP (lift m)

parseSOI :: BParser Word16
parseSOI = do
  satisfy "JPEG magic number" 0xFFD8 =<< liftP getWord16be
  satisfy_ "SOI suffix" 0xFFE1 =<< liftP getWord16be
  liftP getWord16le

