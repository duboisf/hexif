{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif.Parser (
    parseExif
  , ParserResult
  ) where

import Control.Monad (forM, forM_, liftM, unless, void)
import Control.Monad.Writer.Lazy (runWriterT, WriterT, tell)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Error (Error(..), ErrorT, lift, runErrorT, throwError)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Binary.Get as G
import Data.ByteString.Lazy (ByteString)
import Data.Char (toUpper)
import Data.Exif.Types
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word8, Word16, Word32)
import Numeric (showHex)
import Prelude hiding (log)

data ParserError
  = Unsatisfied String
  | ParserError
  | InvalidExifTag Int64 Word16
  | InvalidExifType Int64 Word16
  | UndefinedEndianness String
  | InvalidOffset
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
    msgList = ["Expected", desc, expectedString, "got", toHex actual]
    expectedString = "(" ++ toHex expected ++ ")"
    errorMsg = unwords msgList

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

bytesRead' :: Parser Int64
bytesRead' = liftP G.bytesRead

log :: String -> Parser ()
log x = tell [x]

toHex :: (Show a, Integral a) => a -> String
toHex x = "0x" ++ map toUpper (showHex x "")

logWithPosition :: String -> Parser ()
logWithPosition x = do
  position <- liftP G.bytesRead
  let pos = show position
  let hexPos = toHex position
  log $ concat [x, " (current position: ", pos, " (", hexPos, "))"]

logPosition :: Parser ()
logPosition = do
  position <- liftP G.bytesRead
  log $ "Current position: " ++ toHex position

{-
 - PARSERS
 -
 - Here follows all our binary parsers for the Exif format.
 - All the parsing methods start with 'p'
 -}

parseByteOrder :: Parser Endianness
parseByteOrder = do
  rawWord <- getW16be
  logWithPosition "parsed byte order tag"
  case rawWord of
    0x4949 -> return LittleEndian
    0x4D4D -> return BigEndian
    other -> throwError $ UndefinedEndianness $ toHex other

-- Yup, we expect this following the Endianness in the header
parse42 :: Parser ()
parse42 = satisfy_ "42" 0x002A =<< getW16le

-- SOI: Start Of Image
parseSOI :: Parser ()
parseSOI = satisfy_ "JPEG magic number" 0xFFD8 =<< getW16be

-- Used to abstract result type so that when testing I don't need to change 15
-- method signatures
type TempResult = (TIFFHeader, [RawIFDField])

parseApp1 :: Parser (TIFFHeader, [RawIFDField])
parseApp1 = do
  satisfy_ "APP1 marker" 0xFFE1 =<< getW16be
  logWithPosition "parsed APP1 marker"
  app1Length <- getW16le
  logWithPosition "parsed app1 length"
  satisfy_ "Exif identifier code" 0x45786966 =<< getW32be
  logWithPosition "parsed exif identifier code"
  satisfy_ "Byte order prefix" 0x00 =<< getW16le
  logWithPosition "parsed byte order prefix"

  -- Parse TIFF header
  tiffHeader <- parseTIFFHeader
  log $ "TIFF header offset: " ++ toDecimHex (thOffset tiffHeader)
  liftP $ G.skip $ fromInteger $ toInteger $ thIFDOffset tiffHeader
  logWithPosition "skiped to ifd0 offset"

  -- Parse raw fields
  (raw0thIFDFields, nextIFDOffset) <- parse0thIFD
  let nbFields = length raw0thIFDFields
  log $ "finished parsing the 0th IFD, found " ++ show nbFields ++ " fields"
  log $ "next IFD offset: " ++ toHex nextIFDOffset
  parseLongFieldValues tiffHeader raw0thIFDFields
  return (tiffHeader, raw0thIFDFields)

-- fields with values bigger than 4 bytes need special handling,
-- since the actual values are offset farther down the bytestream
-- instead of being contained in the field definition.
parseLongFieldValues :: TIFFHeader -> [RawIFDField] -> Parser [IFDField]
parseLongFieldValues header rawFields = do
  -- filter to keep only the fields with values bigger than 4 bytes,
  -- then order by the offset value, as we can't rewind the bytestream
  let sortedFields = sortBy (comparing rifOffset) $ filter sizeBiggerThan4 rawFields
  let tiffOffset = thOffset header
  forM sortedFields $ \rawField -> do
    let valueOffset = tiffOffset + rifOffset rawField
    currentOffset <- fromIntegral `liftM` bytesRead'
    unless (valueOffset == currentOffset) $
      logError InvalidOffset $ concat [
          "expecting offset ", toHex valueOffset, ", got ", toHex currentOffset
        ]
    logWithPosition "returning new field"
    -- for now just skip the bytes to test
    liftP $ G.skip $ typeSize (rifType rawField) * rifCount rawField
    return undefined
  where
    sizeBiggerThan4 field = typeSize (rifType field) * rifCount field > 4

logError :: ParserError -> String -> Parser ()
logError err msg =
  log ("Error: " ++ show err ++ ": " ++ msg) >> throwError err

parse0thIFD :: Parser ([RawIFDField], Word32)
parse0thIFD = do
  nbFields <- fromIntegral `liftM` getW16le
  logWithPosition $ "parsed nb fields, it's " ++ show nbFields
  rawFields <- parseRawFields nbFields []
  nextIFDOffset <- getW32le
  return (rawFields, nextIFDOffset)
  where
    parseRawFields :: Int -> [RawIFDField] -> Parser [RawIFDField]
    parseRawFields 0 res = return $ reverse res
    parseRawFields n cum = do
      log $ "parsing field " ++ show (length cum)
      logLine
      field <- praseRawFieldDef
      logLine
      parseRawFields (n - 1) (field : cum)
    logLine = log $ replicate 20 '-'

{-
 - This results in raw field definitions. The difference between a raw field
 - def and a field def is that raw field defs contain an offset, which can
 - possibly be the actual value of the field.  To determine if the offset is
 - the actual value, the size of the value needs to be 4 bytes or less. The
 - size of the value is the count (nb of values) times the size of each value.
 -}
praseRawFieldDef :: Parser RawIFDField
praseRawFieldDef = do
  tag <- getW16le
  logWithPosition $ "tag: " ++ toHex tag ++ " (" ++ show (word2Tag tag) ++ ")"
  rawType <- getW16le
  exifType <- case word2ExifType rawType of
    Nothing -> do
      bytesRead' <- liftP G.bytesRead
      throwError $ InvalidExifType bytesRead' rawType
    Just y -> return y
  logWithPosition $ "exifType: " ++ show exifType
  rawCount <- getW32le
  let count = fromIntegral rawCount
  logWithPosition $ "count: " ++ show count
  valueOffset <- getW32le
  logWithPosition $ "value offset: " ++ toDecimHex valueOffset
  return $ RawIFDField tag exifType count valueOffset

toDecimHex :: (Integral a, Show a) => a -> String
toDecimHex x = show x ++ " (" ++ toHex x ++ ")"

parseTIFFHeader :: Parser TIFFHeader
parseTIFFHeader = do
  tiffHeaderOfset <- (fromIntegral . toInteger) `liftM` liftP G.bytesRead
  log $ "TIFF header offset: " ++ toHex tiffHeaderOfset
  endianness <- parseByteOrder
  parse42
  logWithPosition "parsed 42"
  offset <- parseIFDOffset
  return $ TIFFHeader tiffHeaderOfset endianness offset

parseIFDOffset :: Parser Word32
parseIFDOffset = do
  rawWord <- getW32le
  logWithPosition "parsed ifd0 offset"
  return $ rawWord - 8

{-
 - This is the parsing function that is used to actually parse the Exif tag
 - of an image file. We expect to receive a ByteString of said image as input.
 -}
parseExif :: ByteString -> ParserResult TempResult
parseExif = runParser $ do
  logWithPosition "Starting parsing"
  parseSOI
  logWithPosition "Parsed start of image"
  (header, fields) <- parseApp1
  return (header, fields)

