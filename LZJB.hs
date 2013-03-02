module LZJB (compress, decompress) where

import Data.Bits
import Data.Word
import Data.List

matchBits = 6
matchMin = 3
matchMax = (rotate 1 matchBits) + (matchMin - 1)
offsetMask = (rotate 1 (16 - matchBits)) - 1
lempelSize = 256
nBBy = 8

checksum32 :: [Word8] -> Word32
checksum32 = foldl' (\z e -> (rotate z (0-1)) + (fromIntegral e)) 0
