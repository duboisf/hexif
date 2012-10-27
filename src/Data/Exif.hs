module Data.Exif where

import Data.Binary.Get (Get, getWord16be)
import Data.List (intercalate)
import Numeric (showHex)

satisfy :: (Show a, Eq a) => a -> a -> Get (Either String a)
satisfy expected actual =
  if actual == expected
    then return $ Right actual
    else return $ Left errorMsg
  where
    msgList = ["Expected", show expected, ", got",  show actual] 
    errorMsg = intercalate " " msgList

parseHeader :: Get (Either String ())
parseHeader = do
  soi <- getWord16be
  if soi == 0xFFD8
    then return $ Right ()
    else return $ Left $ "Expected JPEG magic number, read 0x" ++ showHex soi ""

--hex :: a -> String
--hex number = "0x" ++ showHex number ""
  
