{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif.Parser (
    parseExif
  , ParserResult
  ) where

import Control.Monad (liftM, void)
import Control.Monad.Writer.Lazy (runWriterT, WriterT, tell)
import Control.Monad.Writer.Class (MonadWriter)
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
  runP :: ErrorT ParserError (WriterT [String] G.Get) a
} deriving (Functor, Monad, MonadError ParserError, MonadWriter [String])

type ParserResult a = (Either ParserError a, [String])

runParser :: Parser a -> ByteString -> ParserResult a
runParser parser = G.runGet (runWriterT (runErrorT (runP parser)))

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
liftP = Parser . lift . lift

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

currentTiffOffset :: TIFFHeader -> Parser Int
currentTiffOffset header = do
  bytesRead' <- liftP G.bytesRead
  return $ (fromInteger . toInteger) bytesRead' - (thOffset header)

{-
 - PARSERS
 -
 - Here follows all our binary parsers for the Exif format.
 - All the parsing methods start with 'p'
 -}

parseByteOrder :: Parser Endianness
parseByteOrder = do
  rawWord <- getW16be
  case rawWord of
    0x4949 -> return LittleEndian
    0x4D4D -> return BigEndian
    other -> throwError $ UndefinedEndianness $ showHex other ""

-- Yup, we expect this following the Endianness in the header
parse42 :: Parser ()
parse42 = satisfy_ "42" 0x002A =<< getW16le

-- SOI: Start Of Image
parseSOI :: Parser ()
parseSOI = satisfy_ "JPEG magic number" 0xFFD8 =<< getW16be

-- Used to abstract result type so that when testing I don't need to change 15
-- method signatures
type TempResult = (TIFFHeader, [RawIFDField], Int64)

parseApp1 :: Parser (TIFFHeader, [RawIFDField])
parseApp1 = do
  satisfy_ "APP1 marker" 0xFFE1 =<< getW16be
  app1Length <- getW16le
  satisfy_ "Exif identifier code" 0x45786966 =<< getW32be
  satisfy_ "Byte order prefix" 0x00 =<< getW16le
  tiffHeader <- parseTIFFHeader
  liftP $ G.skip $ thIFDOffset tiffHeader
  tell ["about to parse the 0th IFD"]
  (raw0thIFDFields, nextIFDOffset) <- parse0thIFD
  let nbFields = length raw0thIFDFields
  tell [("finished parsing the 0th IFD, found " ++ show nbFields ++ " fields")]
  return (tiffHeader, raw0thIFDFields)

parse0thIFD :: Parser ([RawIFDField], Word16)
parse0thIFD = do
  nbFields <- getW16le
  rawFields <- repeat (fromIntegral nbFields) []
  nextIFDOffset <- getW16le
  return (rawFields, nextIFDOffset)
  where
    repeat :: Int -> [RawIFDField] -> Parser [RawIFDField]
    repeat 0 res = return $ reverse res
    repeat n cum = do
      field <- praseRawFieldDef
      repeat (n - 1) (field : cum)

{-
 - This results in raw field definitions. The difference between a raw field
 - def and a field def is that raw field defs contain an offset, which can
 - possibly be the actual value of the field.  To determine if the offset is
 - the actual value, the size of the value needs to be 4 bytes or less. The
 - size of the value is the count (nb of values) times the size of each value.
 -}
praseRawFieldDef :: Parser RawIFDField
praseRawFieldDef = do
  tag <- word2Tag `liftM` getW16le
  rawType <- getW16le
  exifType <- case word2ExifType rawType of
    Nothing -> do
      bytesRead' <- liftP G.bytesRead
      throwError $ InvalidExifType bytesRead' rawType
    Just y -> return y
  rawCount <- getW32le
  let count = fromIntegral rawCount
  valueOffset <- getW32le
  return $ RawIFDField tag exifType count valueOffset

parseTIFFHeader :: Parser TIFFHeader
parseTIFFHeader = do
  tiffHeaderOfset <- (fromIntegral . toInteger) `liftM` liftP G.bytesRead
  endianness <- parseByteOrder
  parse42
  offset <- parseIFDOffset
  return $ TIFFHeader tiffHeaderOfset endianness offset

parseIFDOffset :: Parser Int
parseIFDOffset = do
  rawWord <- getW32le
  return $ fromIntegral $ toInteger (rawWord - 8)

{-
 - This is the parsing function that is used to actually parse the Exif tag
 - of a image file. We expect to receive a ByteString of said image.
 -}
parseExif :: ByteString -> ParserResult TempResult
parseExif = runParser $ do
  parseSOI
  (header, fields) <- parseApp1
  position <- liftP G.bytesRead
  return (header, fields, position)
