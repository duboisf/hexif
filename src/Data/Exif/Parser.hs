{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif.Parser where

import Control.Monad (void)
import Control.Monad.Error (Error(..), ErrorT, lift, runErrorT, throwError)
import Control.Monad.Error.Class (MonadError)
import Data.Binary.Get (Get, getWord16be, getWord16le, getWord32be, getWord32le, runGet)
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
  runP :: ErrorT ParserError Get a
} deriving (Functor, Monad, MonadError ParserError)

runParser :: Parser a -> ByteString -> Either ParserError a
runParser parser = runGet (runErrorT (runP parser))

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

-- liftP is to lift the Get monad into our Parser (liftP => lift parser)
liftP :: Get a -> Parser a
liftP m = Parser (lift m)

getW16be :: Parser Word16
getW16be = liftP getWord16be

getW16le :: Parser Word16
getW16le = liftP getWord16le

getW32le :: Parser Word32
getW32le = liftP getWord32le

getW32be :: Parser Word32
getW32be = liftP getWord32be

-- Here follows all our binary parsers for the Exif format.
-- All the parsing methods start with 'p'

pByteOrder :: Parser Endianness
pByteOrder = do
  rawWord <- getW16be
  case rawWord of
    0x4949 -> return LittleEndian
    0x4D4D -> return BigEndian
    other -> throwError $ UndefinedEndianness $ showHex other ""

-- Yup, we expect this following the Endianness in the header
p42 :: Parser ()
p42 = satisfy_ "42" 0x002A =<< getW16le

pSOI :: Parser ()
pSOI = do
  satisfy_ "JPEG magic number" 0xFFD8 =<< getW16be
  satisfy_ "SOI suffix" 0xFFE1 =<< getW16be
  getW16be
  satisfy_ "Exif string" 0x45786966 =<< getW32be
  satisfy_ "Byte order prefix" 0x00 =<< getW16le

pIFDOffset :: Parser Int
pIFDOffset = do
  rawWord <- getW32le
  return $ fromIntegral $ toInteger (rawWord - 8)

parseExif :: ByteString -> Either ParserError ()
parseExif = runParser (pSOI >> pByteOrder >> void p42)
