module Data.Exif.Types where

import Data.Ratio (Ratio)
import Data.Word (Word16, Word32)

data Endianness = LittleEndian | BigEndian deriving (Show)

data TIFFHeader = TIFFHeader {
    thByteOrder :: Endianness
  , thIFDOffset :: Int
} deriving (Show)

data IFDField = IFDField {
    ifTag :: Tag
  , ifType :: ExifType
  , ifCount :: Word32
  , ifOffset :: Word32
} deriving Show

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
  | TExif
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
    0x8769 -> TExif
    0x8825 -> TGPS
    0xA005 -> TInteroperability
    -- Handle unknow and private tags
    _  ->
      if rawWord >= 32768
        then TUnknownPrivateTag rawWord
        else TUnknownTag rawWord

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

data ExifType
  = Byte
  | Ascii
  | Short
  | Long
  | ExifRational
  | Undefined
  | SLong
  | SRational
    deriving (Show)

word2ExifType :: Word16 -> Maybe ExifType
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

