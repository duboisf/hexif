module Data.Exif where

import Data.Binary.Get (Get, getWord16be)
import Numeric (showHex)

satisfy :: (Show a, Eq a, Integral a) => String -> a -> a -> Either String a
satisfy desc expected actual =
  if actual == expected
    then Right actual
    else Left errorMsg
  where
    msgList = ["Expected", desc, expectedString, "got", showHex' actual]
    expectedString = "(" ++ showHex' expected ++ ")"
    errorMsg = unwords msgList
    showHex' :: (Integral a, Show a) => a -> String
    showHex' number = "0x" ++ showHex number ""

satisfy_ :: (Show a, Eq a, Integral a) => String -> a -> a -> Either String ()
satisfy_ desc expected actual =
  case satisfy desc expected actual of
    Right _ -> Right ()
    Left msg -> Left msg

parseSOI :: Get (Either String ())
parseSOI =
  return . satisfy_ "JPEG magic number" 0xFFD8 =<< getWord16be

