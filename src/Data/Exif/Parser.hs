{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Exif.Parser (
    parseExif
  , ParserResult
  ) where

import Control.Monad (ap, forM, liftM, unless, when, void)
import Control.Monad.Writer.Lazy (runWriterT, WriterT, tell)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Error (Error(..), ErrorT, lift, runErrorT, throwError)
import Control.Monad.Error.Class (MonadError)
import qualified Data.Binary.Get as G
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Data.Char (toUpper)
import Data.Exif.Types
import Data.Int (Int64)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Ratio ((%))
import Data.Time (LocalTime, parseTime)
import Data.Word (Word8, Word16, Word32)
import Numeric (showHex)
import Prelude hiding (log)
import System.Locale (defaultTimeLocale)

data ParserError
  = Unsatisfied String
  | ParserError
  | InvalidExifTag Int64 Word16
  | InvalidExifType Int64 Word16
  | InvalidDataTypeError Type String
  | InvalidDateFormat String
  | InvalidOffsetError Word32 String
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

getByte :: Parser Int
getByte = fromIntegral `liftM` getW8

getW16be :: Parser Word16
getW16be = liftP G.getWord16be

getW16le :: Parser Word16
getW16le = liftP G.getWord16le

getShort :: Parser Int
getShort = fromIntegral `liftM` getW16le

getW32le :: Parser Word32
getW32le = liftP G.getWord32le

getW32be :: Parser Word32
getW32be = liftP G.getWord32be

bytesRead' :: Parser Int64
bytesRead' = liftP G.bytesRead

skipBytes :: Int -> Parser ()
skipBytes = liftP . G.skip

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
type TempResult = (TIFFHeader, [ExifTag])

parseApp1 :: Parser TempResult
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
  skipBytes $ fromInteger $ toInteger $ thIFDOffset tiffHeader
  logWithPosition "skiped to ifd0 offset"

  -- Parse raw fields
  (raw0thIFDFields, nextIFDOffset) <- parse0thIFD
  let nbFields = length raw0thIFDFields
  log $ "finished parsing the 0th IFD, found " ++ show nbFields ++ " fields"
  log $ "next IFD offset: " ++ toHex nextIFDOffset
  fields <- parseLongFieldValues tiffHeader raw0thIFDFields
  return (tiffHeader, fields)

-- fields with values bigger than 4 bytes need special handling,
-- since the actual values are offset farther down the bytestream
-- instead of being contained in the field definition.
parseLongFieldValues :: TIFFHeader -> [IFDField] -> Parser [ExifTag]
parseLongFieldValues header rawFields = do
  -- order by the offset value, as we can't rewind the bytestream
  --let sortedFields = sortBy (comparing rifOffset) rawFields
  let tiffOffset = thOffset header
  forM rawFields $ \rawField -> do
    logWithPosition ("processing tag " ++ toHex (rifTag rawField))
    let valueOffset = tiffOffset + rifOffset rawField
    currentOffset <- fromIntegral `liftM` bytesRead'
    logWithPosition ("need: " ++ toHex valueOffset)
    when (sizeBiggerThan4 rawField) $ do
      let skipCount = fromInteger . toInteger $ (valueOffset - currentOffset)
      logWithPosition ("skipping " ++ show skipCount)
      skipBytes skipCount
      logPosition
    currentOffset <- fromIntegral `liftM` bytesRead'
    unless (valueOffset == currentOffset || not (sizeBiggerThan4 rawField)) $
      logError InvalidOffset $ concat [
          "expecting offset ", toHex valueOffset, ", got ", toHex currentOffset
        ]
    field <- readExifTag rawField
    logWithPosition $ "read field: " ++ show field
    return field
  where
    sizeBiggerThan4 field = typeSize (rifType field) * rifCount field > 4

-- The mother of all parsing functions
readExifTag :: IFDField -> Parser ExifTag
readExifTag (IFDField rawTag dataType count offset) =
  case rawTag of
    0x0100 -> case dataType of
      Short -> toShortTag ImageWidth
      Long -> toLongTag ImageWidth
      _ -> invalidType "ImageWidth"
    0x0101 -> case dataType of
      Short -> toShortTag ImageLength
      Long -> toLongTag ImageLength
      _ -> invalidType "ImageLength"
    0x0102 -> BitsPerSample `liftM` getShort `ap` getShort `ap` getShort
    0x0103 -> return $ Compression intOffset
    0x0106 -> PhotometricInterpretation `liftM` case intOffset of
      2 -> return RGB
      6 -> return YCbCr
      _ -> invalidOffset "PhotometricInterpretation"
    0x0112 -> Orientation `liftM` case intOffset of
      1 -> return ZeroethRowIsTopZeroethColumnIsLeft
      2 -> return ZeroethRowIsTopZeroethColumnIsRight
      3 -> return ZeroethRowIsBottomZeroethColumnIsRight
      4 -> return ZeroethRowIsBottomZeroethColumnIsLeft
      5 -> return ZeroethRowIsLeftZeroethColumnIsTop
      6 -> return ZeroethRowIsRightZeroethColumnIsTop
      7 -> return ZeroethRowIsRightZeroethColumnIsBottom
      8 -> return ZeroethRowIsLeftZeroethColumnIsBottom
      _ -> throwError $ InvalidOffsetError offset "Orientation"
    -- -> SamplesPerPixel Int
    -- -> PlanarConfiguration Int
    -- -> YCbCrSubSampling Int Int
    0x0213 -> YCbCrPositioning `liftM` case intOffset of
      1 -> return Centered
      2 -> return CoSited
      _ -> throwError $ InvalidOffsetError offset "YCbCrPositioning"
    0x011A -> do
      num <- fromIntegral `liftM` getW32le
      denom <- fromIntegral `liftM` getW32le
      return $ XResolution (num % denom)
    0x011B -> do
      num <- fromIntegral `liftM` getW32le
      denom <- fromIntegral `liftM` getW32le
      return $ YResolution (num % denom)
    0x0128 -> ResolutionUnit `liftM` case intOffset of
      2 -> return Centimeters
      3 -> return Inches
      _ -> throwError $ InvalidOffsetError offset "ResolutionUnit"
    -- -- Recording offset
    -- -> StripOffsets Integer
    -- -> RowsPerStrip Integer
    -- -> StripByteCount [Integer]
    -- -> JPEGInterchangeFormat Integer
    -- -> JPEGInterchangeFormatLength Integer
    -- -- Image data characteristics
    -- -> TransferFunction [Int]
    -- -> WhitePoint Rational Rational Rational
    -- -> PrimaryChromaticities [Rational]
    -- -> YCbCrCoefficients Rational Rational Rational
    -- -> ReferenceBlackWhite [Rational]
    -- Other tags
    0x0132 -> makeDateTimeTag
    -- -> ImageDescription String
    0x010F -> makeStringTag Make
    0x0110 -> makeStringTag Model
    0x0131 -> makeStringTag Software
    0x013B -> makeStringTag Artist
    0x8298 -> makeStringTag Copyright
    -- -> SamplesPerPixel Int
    -- -> PlanarConfiguration Int
    -- -> YCbCrSubSampling Int Int
    -- -> YCbCrPositioning Int
    -- -> XResolution Rational
    -- -> YResolution Rational
    -- -> ResolutionUnit ResolutionUnit
    -- -- Recording offset
    -- -> StripOffsets Integer
    -- -> RowsPerStrip Integer
    -- -> StripByteCount [Integer]
    -- -> JPEGInterchangeFormat Integer
    -- -> JPEGInterchangeFormatLength Integer
    -- -- Image data characteristics
    -- -> TransferFunction [Int]
    -- -> WhitePoint Rational Rational Rational
    -- -> PrimaryChromaticities [Rational]
    -- -> YCbCrCoefficients Rational Rational Rational
    -- -> ReferenceBlackWhite [Rational]
    {-
    - Exif Attributes
    -}
    -- Version
    -- -> ExifVersion Word32
    -- -> FlashPixVersion Word32
    -- Image data characteristics
    -- -> ColorSpace Int
    -- -> ComponentsConfiguration Word32
    -- -> CompressedBitsPerPixel Rational
    -- -> PixelXDimension Integer
    -- -> PixelYDimension Integer
    -- User information
    -- -> MakerNote String
    -- -> UserComment String
    -- Related file information
    -- -> RelatedSoundFile String
    -- Date and time
    -- -> DateTimeOriginal String
    -- -> DateTimeDigitized String
    -- -> SubSecTime String
    -- -> SubSecTimeOriginal String
    -- -> SubSecTimeDigitized String
    -- Picture-taking conditions
    -- -> ExposureTime Rational
    -- -> FNumber Rational
    -- -> ExposureProgram Int
    -- -> SpectralSensitivity String
    -- -> ISOSpeedRatings Int
    -- -> OECF Word32
    -- -> ShutterSpeedValue Rational
    -- -> ApertureValue Rational
    -- -> BrightnessValue Rational
    -- -> ExposureBiasValue Rational
    -- -> MaxApertureValue Rational
    -- -> SubjectDistance Rational
    -- -> MeteringMode Int
    -- -> LightSource Int
    -- -> Flash Int
    -- -> FocalLength Rational
    -- -> SubjectArea [Int]
    -- -> FlashEnergy Rational
    -- -> SpatialFrequencyResponse [Word8]
    -- -> FocalPlaneXResolution Rational
    -- -> FocalPlaneYResolution Rational
    -- -> FocalPlaneResolutionUnit Int
    -- -> SubjectLocation Int Int
    -- -> ExposureIndex Rational
    -- -> SensingMethod Int
    -- -> FileSource Word32
    -- -> SceneType Word32
    -- -> CFAPattern [Word8]
    -- -> CustomRenderer Int
    -- -> ExposureMode Int
    -- -> WhiteBalance Int
    -- -> DigitalZoomRatio Rational
    -- -> FocalLengthIn35mmFilm Int
    -- -> SceneCaptureType Int
    -- -> GainControl Rational
    -- -> Contrast Int
    -- -> Saturation Int
    -- -> Sharpness Int
    -- -> DeviceSettingsDescription [Word8]
    -- -> SubjectDistanceRange Int
    -- Other
    -- -> ImageUniqueID String
    _ -> return $ if rawTag >= 32768
        then UnknownPrivateTag rawTag
        else UnknownTag rawTag

  where

    between :: Ord a => a -> a -> a -> Bool
    between n x y = n >= x && n <= y

    intOffset :: Int
    intOffset = fromIntegral offset

    invalidType :: String -> Parser a
    invalidType = throwError . InvalidDataTypeError dataType

    invalidOffset :: String -> Parser a
    invalidOffset = throwError . InvalidOffsetError offset

    makeDateTimeTag :: Parser ExifTag
    makeDateTimeTag = do
      rawDateTime <- (dropRightNulls . unpack) `liftM` (liftP . G.getLazyByteString $ fromIntegral count)
      case parseTime' rawDateTime of
        Just dateTime -> return $ DateTime dateTime
        otherwise -> throwError $ InvalidDateFormat rawDateTime
      where
        parseTime' :: String -> Maybe LocalTime
        parseTime' =
          parseTime defaultTimeLocale "%Y:%m:%d %H:%M:%S"

    makeStringTag :: (String -> ExifTag) -> Parser ExifTag
    makeStringTag ctor = do
      bytestring <- liftP . G.getLazyByteString $ fromIntegral count
      return $ ctor . dropRightNulls . unpack $ bytestring

    dropRightNulls :: String -> String
    dropRightNulls = takeWhile ((/=) '\NUL')

    makeIntTag :: Integral a => Parser a -> (Int -> ExifTag) -> Parser ExifTag
    makeIntTag parser ctor = do
      int <- fromIntegral `liftM` parser
      return $ ctor int

    parseShortTag :: (Int -> ExifTag) -> Parser ExifTag
    parseShortTag = makeIntTag getW16le

    toShortTag :: (Int -> ExifTag) -> Parser ExifTag
    toShortTag = makeIntTag (return offset)

    parseLongTag :: (Int -> ExifTag) -> Parser ExifTag
    parseLongTag = makeIntTag getW32le

    toLongTag :: (Int -> ExifTag) -> Parser ExifTag
    toLongTag = makeIntTag (return offset)

logError :: ParserError -> String -> Parser ()
logError err msg =
  log ("Error: " ++ show err ++ ": " ++ msg) >> throwError err

parse0thIFD :: Parser ([IFDField], Word32)
parse0thIFD = do
  nbFields <- fromIntegral `liftM` getW16le
  logWithPosition $ "parsed nb fields, it's " ++ show nbFields
  rawFields <- parseRawFields nbFields []
  nextIFDOffset <- getW32le
  return (rawFields, nextIFDOffset)
  where
    parseRawFields :: Int -> [IFDField] -> Parser [IFDField]
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
praseRawFieldDef :: Parser IFDField
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
  return $ IFDField tag exifType count valueOffset

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

