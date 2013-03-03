module LZJB (compress, decompress, compressNaive) where

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



-- naivete follows.

data OutputToken = EOF | LiteralT !Word8 | PhraseT !Word8 !Word8
data ChunkedOutputToken = Chunk !Word8 !(!OutputToken,!OutputToken,!OutputToken,!OutputToken,!OutputToken,!OutputToken,!OutputToken,!OutputToken)

dumpChunks :: [ChunkedOutputToken] -> [Word8]
dumpChunks [] = []
dumpChunks ((Chunk litPhraseBits (a,b,c,d,e,f,g,h)):cs) = litPhraseBits : (concatMap dumpToken [a,b,c,d,e,f,g,h] ++ dumpChunks cs)

dumpToken :: OutputToken -> [Word8]
dumpToken EOF = []
dumpToken (LiteralT w) = [w]
dumpToken (PhraseT  w1 w2) = [w1,w2]

chunkTokens :: [OutputToken] -> [ChunkedOutputToken]
chunkTokens [] = []
chunkTokens outputTokens | null (drop 8 outputTokens) = chunkTokens (outputTokens ++ [EOF])
chunkTokens (a:b:c:d:e:f:g:h:tokens) = foldl' (\ litPhraseBits (token,idx) -> (if isPhrase token then setBit else clearBit) litPhraseBits idx) 0 (zip [a,b,c,d,e,f,g,h] [0..7])

isPhrase :: OutputToken -> Bool
isPhrase (PhraseT _ _) = True
isPhrase _ = False

tokenizeInput :: [Word8] -> [Word8] -> [OutputToken]
tokenizeInput _ [] = []
tokenizeInput _ [w] = [LiteralT w]
tokenizeInput _ [w1,w2] = [LiteralT w1, LiteralT w2]
tokenizeInput window input = (if matchLength > 2 then (PhraseT w1 w2) else (LiteralT lit)) : (tokenizeInput nextWindow nextInput)
  where (matchLength, position) = getLongestMatch window input
        worthEncodingPhrase = matchLength > 2
        finalMatchLength = if worthEncodingPhrase then matchLength else 1
        nextWindow = (drop finalMatchLength window) ++ (take finalMatchLength input)
        nextInput = (drop finalMatchLength input)
        (w1, w2) = serializeMatchLengthPosition matchLength position
        lit = head input
