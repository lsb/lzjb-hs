{-# LANGUAGE BangPatterns #-}
--module LZJB (compress, decompress) where
import Data.Bits ((.&.), (.|.), shift, bit)
import Data.Word (Word8)
import Data.Int (Int8, Int16)
import Data.Ord (comparing)
import Data.List (maximumBy, tails)
import System.Environment (getArgs)
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import qualified Data.Vector.Unboxed as V
-- This is bit-compatible with LZJB, but different algorithms, optimized for legibility and purity.

bitsPerByte = 8
phraseBits = 16
matchBits = 6
offsetBits = phraseBits - matchBits
matchMin = (phraseBits `div` bitsPerByte) + 1
matchMax = (bit matchBits) + (matchMin - 1)
windowMax = bit offsetBits - 1
initialWindow = S.empty

data Token = EOF | LiteralT !Word8 | PhraseT !Word8 !Word8 deriving (Show, Eq)
data ChunkedTokens = Chunk !Word8 !Token !Token !Token !Token !Token !Token !Token !Token deriving Show

lengthenTo :: S.Seq a -> Int -> S.Seq a
lengthenTo s len = if slen == len then s else (if slen >= len then S.take len s else s S.>< lengthenTo s (len - slen))
  where slen = S.length s

compress :: [Word8] -> [Word8]
compress = dumpChunks . chunkTokens . tokenizeInput initialWindow

dumpChunks :: [ChunkedTokens] -> [Word8]
dumpChunks [] = []
dumpChunks ((Chunk litPhraseBits a b c d e f g h):cs) = litPhraseBits : (concatMap dumpToken [a,b,c,d,e,f,g,h] ++ dumpChunks cs)

dumpToken :: Token -> [Word8]
dumpToken EOF = []
dumpToken (LiteralT w) = [w]
dumpToken (PhraseT  w1 w2) = [w1,w2]

chunkTokens :: [Token] -> [ChunkedTokens]
chunkTokens [] = []
chunkTokens outputTokens | null (drop 7 outputTokens) = chunkTokens (outputTokens ++ [EOF])
chunkTokens (a:b:c:d:e:f:g:h:tokens) = (Chunk litPhrase a b c d e f g h) : chunkTokens tokens
  where litPhrase = foldr (\ token litPhraseBits -> litPhraseBits * 2 + phraseToBit token) 0 [a,b,c,d,e,f,g,h]

phraseToBit :: Token -> Word8
phraseToBit (PhraseT _ _) = 1
phraseToBit _ = 0

tokenizeInput :: S.Seq Word8 -> [Word8] -> [Token]
tokenizeInput _      input | null (drop (matchMin-1) input) = map LiteralT input
tokenizeInput window input = (if worthEncodingPhrase then (PhraseT w1 w2) else (LiteralT lit)) : (tokenizeInput nextWindow nextInput)
  where (!matchLength, !position) = getLongestMatch window input
        worthEncodingPhrase = matchLength >= matchMin
        finalMatchLength = if worthEncodingPhrase then matchLength else 1
        nextWindow = (S.drop (finalMatchLength + S.length window - windowMax) window) S.>< (S.fromList (take finalMatchLength input))
        nextInput = (drop finalMatchLength input)
        (w1, w2) = serializeMatchLengthPosition matchLength position
        lit = head input

getLongestMatch :: S.Seq Word8 -> [Word8] -> (Int,Int)
getLongestMatch window _ | S.null window = (0,0)
getLongestMatch window input = V.maximumBy (comparing fst) (V.generate windowLength (\ subWindowIdx -> (commonPrefixLengthCirc 0 subWindowIdx input, windowLength - subWindowIdx) ))
-- getLongestMatch window input = getLongestMatchMaxByFstEnumFrom window input 0 0 (S.length window - 1)
  where windowLength = S.length window
        commonPrefixLengthCirc !len !minIdxX ys | null ys || len == matchMax = len
        commonPrefixLengthCirc !len !minIdxX (y:ys) = if window `S.index` (minIdxX + len `mod` (windowLength - minIdxX)) == y then commonPrefixLengthCirc (len+1) minIdxX ys else len


getLongestMatchMaxByFstEnumFrom :: S.Seq Word8 -> [Word8] -> Int -> Int -> Int -> (Int,Int)
getLongestMatchMaxByFstEnumFrom !window !input !matchLength !position !idx = if idx < 0 then (matchLength, windowLength - position) else let !len = commonPrefixLengthCirc 0 idx input in getLongestMatchMaxByFstEnumFrom window input (if len > matchLength then len else matchLength) (if len > matchLength then idx else position) (idx-1)
  where windowLength = S.length window
        commonPrefixLengthCirc !len !minIdxX ys | null ys || len == matchMax = len
        commonPrefixLengthCirc !len !minIdxX (y:ys) = if window `S.index` (minIdxX + len `mod` (windowLength - minIdxX)) == y then commonPrefixLengthCirc (len+1) minIdxX ys else len

commonPrefixLengthCircular :: Int -> Int -> S.Seq Word8 -> [Word8] -> Int
commonPrefixLengthCircular !len !minIdxX xs ys | null ys || len == matchMax = len
commonPrefixLengthCircular !len !minIdxX xs (y:ys) = if xs `S.index` (minIdxX + len `mod` (S.length xs - minIdxX)) == y then commonPrefixLengthCircular (len+1) minIdxX xs ys else len

serializeMatchLengthPosition :: Int -> Int -> (Word8,Word8)
serializeMatchLengthPosition matchLength position = (matchLengthBits .|. highPositionBits, lowPositionBits)
  where biasedMatchLength = fromIntegral (matchLength - matchMin)
        matchLengthBits = shift biasedMatchLength (bitsPerByte - matchBits)
        highPositionBits = fromIntegral (shift position (0-bitsPerByte))
        lowPositionBits = (fromIntegral position) :: Word8




decompress :: [Word8] -> [Word8]
decompress = expandPhrases initialWindow . readTokens

readTokens :: [Word8] -> [Token]
readTokens input = readLitPhrase input

readLitPhrase [] = []
readLitPhrase (w:ws) = readChunk w 0 ws

readChunk 0 8 ws = readLitPhrase ws
readChunk 0 _ [] = []
readChunk litPhrase idx (w:ws)     | even litPhrase = (LiteralT w)    : readChunk (shift litPhrase (-1)) (idx + 1) ws
readChunk litPhrase idx (w1:w2:ws) | odd litPhrase  = (PhraseT w1 w2) : readChunk (shift litPhrase (-1)) (idx + 1) ws
readChunk _ _ _ = error "bad lz stream"

expandPhrases :: S.Seq Word8 -> [Token] -> [Word8]
expandPhrases _ [] = []
expandPhrases window (EOF:ts) = error "internal lz error"
expandPhrases window ((LiteralT w):ts) = w : (expandPhrases ((if S.length window < windowMax then window else S.drop 1 window) S.|> w) ts)
expandPhrases window ((PhraseT w1 w2):ts) = if 0 == position then error "bad zero-length phrase" else (F.toList phrase) ++ (expandPhrases newWindow ts)
  where !windowSize = S.length window
        phrase = (S.drop (windowSize - position) window) `lengthenTo` matchLength
        newWindow = (S.drop (windowSize + matchLength - windowMax) window) S.>< phrase
        (!matchLength, !position) = deserializeMatchLengthPosition w1 w2

deserializeMatchLengthPosition :: Word8 -> Word8 -> (Int,Int)
deserializeMatchLengthPosition !w1 !w2 = (matchLength, position)
  where matchLength = (fromIntegral biasedMatchLength) + matchMin
        biasedMatchLength = shift w1 (matchBits - bitsPerByte)
        highPositionBits = (bit (bitsPerByte - matchBits) - 1) .&. w1
        lowPositionBits = fromIntegral w2
        position = (shift (fromIntegral highPositionBits) bitsPerByte) + lowPositionBits

main = getArgs >>= \ args -> interact (map (toEnum . fromIntegral) . (if null args then compress else decompress) . map (fromIntegral . fromEnum) )