module Data.Exif.Types where

import Data.Ratio (Ratio)
import Data.Word (Word8, Word16, Word32)

data Endianness = LittleEndian | BigEndian deriving (Show)

data TIFFHeader = TIFFHeader {
    thOffset :: Word32
  , thByteOrder :: Endianness
  , thIFDOffset :: Word32
} deriving (Show)

-- IFD (Image File Directory) Field
data RawIFDField = RawIFDField {
    rifTag :: Word16
  , rifType :: Type
  , rifCount :: Int
  , rifOffset :: Word32
} deriving Show

data IFDField = IFDField {
    ifType :: Type
  , ifCount :: Int
  , ifValue :: ExifAttribute
} deriving Show

toIFDField :: RawIFDField -> ExifAttribute -> IFDField
toIFDField rawField = IFDField (rifType rawField) (rifCount rawField)

type RatioW32 = Ratio Word32

data Tag
  -- Tags relating to image data structure
  = TImageWidth
  | TImageLength
  | TBitsPerSample
  | TCompression
  | TPhotometricInterpretation
  | TOrientation
  | TSamplesPerPixel
  | TPlanarConfiguration
  | TYCbCrSubSampling
  | TYCbCrPositioning
  | TXResolution
  | TYResolution
  | TResolutionUnit
  -- Tags relating to recording offset
  | TStripOffsets
  | TRowsPerStrip
  | TStripByteCount
  | TJPEGInterchangeFormat
  | TJPEGInterchangeFormatLength
  -- Tags relating to image data characteristics
  | TTransferFunction
  | TWhitePoint
  | TPrimaryChromaticities
  | TYCbCrCoefficients
  | TReferenceBlackWhite
  -- Other tags
  | TDateTime
  | TImageDescription
  | TMake
  | TModel
  | TSoftware
  | TArtist
  | TCopyright
  -- Tags relating to Exif
  | TExifIFDPointer
  | TGPS
  | TInteroperability
  -- Private and unknow tags
  | TUnknownPrivateTag Word16
  | TUnknownTag Word16
    deriving (Show)

word2Tag :: Word16 -> Tag
word2Tag rawWord =
  case rawWord of
    -- Tags relating to image data structure
    0x0100 -> TImageWidth
    0x0101 -> TImageLength
    0x0102 -> TBitsPerSample
    0x0103 -> TCompression
    0x0106 -> TPhotometricInterpretation
    0x0112 -> TOrientation
    0x0115 -> TSamplesPerPixel
    0x011C -> TPlanarConfiguration
    0x0212 -> TYCbCrSubSampling
    0x0213 -> TYCbCrPositioning
    0x011A -> TXResolution
    0x011B -> TYResolution
    0x0128 -> TResolutionUnit
    -- Tags relating to recording offset
    0x0111 -> TStripOffsets
    0x0116 -> TRowsPerStrip
    0x0117 -> TStripByteCount
    0x0201 -> TJPEGInterchangeFormat
    0x0202 -> TJPEGInterchangeFormatLength
    -- Tags relating to image data characteristics
    0x012D -> TTransferFunction
    0x013E -> TWhitePoint
    0x013F -> TPrimaryChromaticities
    0x0211 -> TYCbCrCoefficients
    0x0214 -> TReferenceBlackWhite
    -- Other tags
    0x0132 -> TDateTime
    0x010E -> TImageDescription
    0x010F -> TMake
    0x0110 -> TModel
    0x0131 -> TSoftware
    0x013B -> TArtist
    0x8298 -> TCopyright
    -- Tags relating to Exif
    0x8769 -> TExifIFDPointer
    0x8825 -> TGPS
    0xA005 -> TInteroperability
    -- Handle unknow and private tags
    _  ->
      if rawWord >= 32768
        then TUnknownPrivateTag rawWord
        else TUnknownTag rawWord

data ExifAttribute
  {-
   - TIFF Attributes used in Exif
   -}
  -- Image data structure
  = ImageWidth Integer
  | ImageLength Integer
  | BitsPerSample Int Int Int
  | Compression Int
  | PhotometricInterpretation Int
  | Orientation Int
  | SamplesPerPixel Int
  | PlanarConfiguration Int
  | YCbCrSubSampling Int Int
  | YCbCrPositioning Int
  | XResolution Rational
  | YResolution Rational
  | ResolutionUnit ResolutionUnit
  -- Recording offset
  | StripOffsets Integer
  | RowsPerStrip Integer
  | StripByteCount [Integer]
  | JPEGInterchangeFormat Integer
  | JPEGInterchangeFormatLength Integer
  -- Image data characteristics
  | TransferFunction [Int]
  | WhitePoint Rational Rational Rational
  | PrimaryChromaticities [Rational]
  | YCbCrCoefficients Rational Rational Rational
  | ReferenceBlackWhite [Rational]
  -- Other tags
  | DateTime String
  | ImageDescription String
  | Make String
  | Model String
  | Software String
  | Artist String
  | Copyright String
  {-
   - Exif Attributes
   -}
  -- Version
  | ExifVersion Word32
  | FlashPixVersion Word32
  -- Image data characteristics
  | ColorSpace Int
  | ComponentsConfiguration Word32
  | CompressedBitsPerPixel Rational
  | PixelXDimension Integer
  | PixelYDimension Integer
  -- User information
  | MakerNote String
  | UserComment String
  -- Related file information
  | RelatedSoundFile String
  -- Date and time
  | DateTimeOriginal String
  | DateTimeDigitized String
  | SubSecTime String
  | SubSecTimeOriginal String
  | SubSecTimeDigitized String
  -- Picture-taking conditions
  | ExposureTime Rational
  | FNumber Rational
  | ExposureProgram Int
  | SpectralSensitivity String
  | ISOSpeedRatings Int
  | OECF Word32
  | ShutterSpeedValue Rational
  | ApertureValue Rational
  | BrightnessValue Rational
  | ExposureBiasValue Rational
  | MaxApertureValue Rational
  | SubjectDistance Rational
  | MeteringMode Int
  | LightSource Int
  | Flash Int
  | FocalLength Rational
  | SubjectArea [Int]
  | FlashEnergy Rational
  | SpatialFrequencyResponse [Word8]
  | FocalPlaneXResolution Rational
  | FocalPlaneYResolution Rational
  | FocalPlaneResolutionUnit Int
  | SubjectLocation Int Int
  | ExposureIndex Rational
  | SensingMethod Int
  | FileSource Word32
  | SceneType Word32
  | CFAPattern [Word8]
  | CustomRenderer Int
  | ExposureMode Int
  | WhiteBalance Int
  | DigitalZoomRatio Rational
  | FocalLengthIn35mmFilm Int
  | SceneCaptureType Int
  | GainControl Rational
  | Contrast Int
  | Saturation Int
  | Sharpness Int
  | DeviceSettingsDescription [Word8]
  | SubjectDistanceRange Int
  -- Other
  | ImageUniqueID String
    deriving (Show)

data ResolutionUnit = Centimeters
                    | Inches
                      deriving Show

-- data TIFFTag =
--                -- Tags relating to image data structure
--                ImageWidth Word32
--              | ImageLength Word32
--              | BitsPerSample Word16 Word16 Word16
--              | Compression Word16
--              | PhotometricInterpretation Word16
--              | Orientation Word16
--              | SamplesPerPixel Word16
--              | PlanarConfiguration Word16
--              | YCbCrSubSampling Word16 Word16
--              | YCbCrPositioning Word16
--              | XResolution Rational
--              | YResolution Rational
--              | ResolutionUnit Word16
--                -- Tags relating to recording offset
--              | StripOffsets [Word32]
--              | RowsPerStrip Word32
--              | StripByteCount [Word32]
--              | JPEGInterchangeFormat Word32
--              | JPEGInterchangeFormatLength Word32
--                -- Tags relating to image data characteristics
--              | TransferFunction [Word16]
--              | WhitePoint RatioW32 RatioW32
--              | PrimaryChromaticities [RatioW32]
--              | YCbCrCoefficients RatioW32 RatioW32 RatioW32
--              | ReferenceBlackWhite [RatioW32]
--                -- Other tags
--              | DateTime String
--              | ImageDescription String
--              | Make String
--              | Model String
--              | Software String
--              | Artist String
--              | Copyright String
--                deriving (Show)

data Type
  = Byte
  | Ascii
  | Short
  | Long
  | ExifRational
  | Undefined
  | SLong
  | SRational
    deriving (Show)

word2ExifType :: Word16 -> Maybe Type
word2ExifType rawWord =
  case rawWord of
    1  -> Just Byte
    2  -> Just Ascii
    3  -> Just Short
    4  -> Just Long
    5  -> Just ExifRational
    7  -> Just Undefined
    9  -> Just SLong
    10 -> Just SRational
    _  -> Nothing

typeSize :: Type -> Int
typeSize etype =
  case etype of
    Byte         -> 1
    Ascii        -> 1
    Short        -> 2
    Long         -> 4
    ExifRational -> 8
    Undefined    -> 1
    SLong        -> 4
    SRational    -> 8
