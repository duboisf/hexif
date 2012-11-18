{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif.Parser where

import Control.Monad (liftM, void)
import Control.Monad.Error (Error(..), ErrorT, lift, runErrorT, throwError)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Binary.Get as G
import Data.ByteString.Lazy (ByteString)
import Data.Exif.Types
import Data.Int (Int64)
import Data.Word (Word8, Word16, Word32)
import Numeric (showHex)

data ParserError
  = Unsatisfied String
  | ParserError
  | InvalidExifTag Int64 Word16
  | InvalidExifType Int64 Word16
  | UndefinedEndianness String
    deriving (Show)

instance Error ParserError where
  noMsg = ParserError
  strMsg = Unsatisfied

newtype Parser a = Parser {
  runP :: ErrorT ParserError G.Get a
} deriving (Functor, Monad, MonadError ParserError)

runParser :: Parser a -> ByteString -> Either ParserError a
runParser parser = G.runGet (runErrorT (runP parser))

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
liftP :: G.Get a -> Parser a
liftP m = Parser $ lift m

getW8 :: Parser Word8
getW8 = liftP G.getWord8

getW16be :: Parser Word16
getW16be = liftP G.getWord16be

getW16le :: Parser Word16
getW16le = liftP G.getWord16le

getW32le :: Parser Word32
getW32le = liftP G.getWord32le

getW32be :: Parser Word32
getW32be = liftP G.getWord32be

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

-- SOI: Start Of Image
pSOI :: Parser ()
pSOI = satisfy_ "JPEG magic number" 0xFFD8 =<< getW16be

-- Used to abstract result type so that when testing I don't need to change 15
-- method signatures
type TempResult = (TIFFHeader, [IFDField], Int64)

pApp1 :: Parser (TIFFHeader, [IFDField])
pApp1 = do
  satisfy_ "APP1 marker" 0xFFE1 =<< getW16be
  app1Length <- getW16le
  satisfy_ "Exif identifier code" 0x45786966 =<< getW32be
  satisfy_ "Byte order prefix" 0x00 =<< getW16le
  tiffHeader <- pTIFFHeader
  liftP $ G.skip $ thIFDOffset tiffHeader
  (ifd0Fields, nextIFDOffset) <- pIFD0
  return (tiffHeader, ifd0Fields)

pIFD0 :: Parser ([IFDField], Word16)
pIFD0 = do
  nbFields <- getW16le
  fields <- repeat (fromIntegral nbFields) []
  nextIFDOffset <- getW16le
  return (fields, nextIFDOffset)
  where
    repeat :: Int -> [IFDField] -> Parser [IFDField]
    repeat 0 res = return $ reverse res
    repeat n cum = do
      field <- pField
      repeat (n - 1) (field : cum)

pField :: Parser IFDField
pField = do
  tag <- word2Tag `liftM` getW16le
  rawType <- getW16le
  exifType <- case word2ExifType rawType of
    Nothing -> do
      bytesRead' <- liftP G.bytesRead
      throwError $ InvalidExifType bytesRead' rawType
    Just y -> return y
  count <- getW32le
  valueOffset <- getW32le
  return $ IFDField tag exifType count valueOffset

pTIFFHeader :: Parser TIFFHeader
pTIFFHeader = do
  tiffHeaderOfset <- (fromIntegral . toInteger) `liftM` liftP G.bytesRead
  endianness <- pByteOrder
  p42
  offset <- pIFDOffset
  return $ TIFFHeader tiffHeaderOfset endianness offset

pIFDOffset :: Parser Int
pIFDOffset = do
  rawWord <- getW32le
  return $ fromIntegral $ toInteger (rawWord - 8)

parseExif :: ByteString -> Either ParserError TempResult
parseExif = runParser $ do
  pSOI
  (header, fields) <- pApp1
  position <- liftP G.bytesRead
  return (header, fields, position)
