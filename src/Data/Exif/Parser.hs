{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif.Parser where

import Control.Monad (void)
import Control.Monad.Error (Error(..), ErrorT, lift, runErrorT, throwError)
import Control.Monad.Error.Class (MonadError)
import Data.Binary.Get (Get, getWord16be, getWord16le, getWord32be, runGet)
import Data.ByteString.Lazy (ByteString)
import Data.Exif.Types
import Data.Word (Word16, Word32)
import Numeric (showHex)

data ParserError = Unsatisfied String
                 | ParserError
                 | UndefinedEndianness String
  deriving (Show)

instance Error ParserError where
  noMsg = ParserError
  strMsg = Unsatisfied

newtype Parser a = Parser {
  runBP :: ErrorT ParserError Get a
} deriving (Functor, Monad, MonadError ParserError)

runBParser :: Parser a -> ByteString -> Either ParserError a
runBParser parser = runGet (runErrorT (runBP parser))

-- functions to simplify parsing expected bytes
satisfy :: (Show a, Eq a, Integral a) => String -> a -> a -> Parser a
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

satisfy_ :: (Show a, Eq a, Integral a) => String -> a -> a -> Parser ()
satisfy_ desc expected actual =
  void $ satisfy desc expected actual

-- lp is to lift the Get monad into our Parser (lp => lift parser)
lp :: Get a -> Parser a
lp m = Parser (lift m)

-- Here follows all our binary parsers for the Exif format.
-- All the parsing methods start with 'p'

pByteOrder :: Parser Endianness
pByteOrder = do
  rawWord <- lp getWord16be
  case rawWord of
    0x4949 -> return LittleEndian
    0x4D4D -> return BigEndian
    other -> throwError $ UndefinedEndianness $ showHex other ""

expectWord16le :: String -> Word16 -> Parser Word16
expectWord16le desc expected =
  satisfy desc expected =<< lp getWord16le

-- Yup, we expect this following the Endianness in the header
p42 :: Parser ()
p42 = satisfy_ "42" 0x002A =<< lp getWord16le

pSOI :: Parser ()
pSOI = do
  satisfy_ "JPEG magic number" 0xFFD8 =<< lp getWord16be
  satisfy_ "SOI suffix" 0xFFE1 =<< lp getWord16be
  void $ lp getWord16le
  satisfy_ "Exif string" (0x45786966 :: Word32) =<< lp getWord32be
  satisfy_ "Byte order prefix" 0x00 =<< lp getWord16le

parseExif :: ByteString -> Either ParserError ()
parseExif = runBParser (pSOI >> pByteOrder >> void p42)
