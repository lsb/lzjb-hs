--module LZJB (compress, decompress) where
import Data.Bits ((.&.), (.|.), shift, bit)
import Data.Word (Word8)
import Data.Int (Int8, Int16)
import Data.Ord (comparing)
import Data.List (maximumBy, tails)
import System.Environment (getArgs)
import Debug.Trace (traceShow)
import Control.Exception (assert)

-- This is bit-compatible with LZJB, but different algorithms, optimized for legibility and purity.

bitsPerByte = 8
phraseBits = 16
matchBits = 6
offsetBits = phraseBits - matchBits
matchMin = (phraseBits `div` bitsPerByte) + 1
matchMax = (bit matchBits) + (matchMin - 1)
windowMax = bit offsetBits - 1
initialWindow = []

data Token = EOF | LiteralT !Word8 | PhraseT !Word8 !Word8 deriving (Show, Eq)
data ChunkedTokens = Chunk !Word8 !Token !Token !Token !Token !Token !Token !Token !Token deriving Show

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

tokenizeInput :: [Word8] -> [Word8] -> [Token]
tokenizeInput _      input | null (drop (matchMin-1) input) = map LiteralT input
tokenizeInput window input = traceShow (window,input) $ assert (not worthEncodingPhrase || w2 > 0) $ (if worthEncodingPhrase then (PhraseT w1 w2) else (LiteralT lit)) : (tokenizeInput nextWindow nextInput)
  where (matchLength, position) = getLongestMatch window input
        worthEncodingPhrase = matchLength >= matchMin
        finalMatchLength = if worthEncodingPhrase then matchLength else 1
        nextWindow = (drop (finalMatchLength + length window - windowMax) window) ++ (take finalMatchLength input)
        nextInput = (drop finalMatchLength input)
        (w1, w2) = serializeMatchLengthPosition matchLength position
        lit = head input

getLongestMatch :: [Word8] -> [Word8] -> (Int,Int)
getLongestMatch [] _ = (0,0)
getLongestMatch window input = maximumBy (comparing fst) matchLengthPositions
  where windowSize = length window
        matchLengthPositions = [(commonPrefixLength (subWindow ++ (take (matchMax - (length subWindow)) input)) input, length subWindow) | subWindow <- tails window, not (null subWindow)]
	commonPrefixLength xs ys = length $ takeWhile id $ zipWith (==) xs ys

serializeMatchLengthPosition :: Int -> Int -> (Word8,Word8)
serializeMatchLengthPosition matchLength position = (matchLengthBits .|. highPositionBits, lowPositionBits)
  where biasedMatchLength = fromIntegral (matchLength - matchMin)
        matchLengthBits = shift biasedMatchLength (bitsPerByte - matchBits)
        highPositionBits = shift (fromIntegral position) (0-bitsPerByte)
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

expandPhrases :: [Word8] -> [Token] -> [Word8]
expandPhrases _ [] = []
expandPhrases window (EOF:ts) = error "internal lz error"
expandPhrases window ((LiteralT w):ts) = w : (expandPhrases ((if length window < windowMax then window else tail window) ++ [w]) ts)
expandPhrases window ((PhraseT w1 w2):ts) = traceShow ("**********",window,w1,w2) $ phrase ++ (expandPhrases (windowTail ++ phrase) ts)
  where phrase = take matchLength $ concat $ repeat windowTail
        windowTail = drop (length window - position) window
        (matchLength, position) = deserializeMatchLengthPosition w1 w2

deserializeMatchLengthPosition :: Word8 -> Word8 -> (Int,Int)
deserializeMatchLengthPosition w1 w2 = (matchLength, position)
  where matchLength = (fromIntegral biasedMatchLength) + matchMin
        biasedMatchLength = shift w1 (matchBits - bitsPerByte)
        highPositionBits = (bit (bitsPerByte - matchBits) - 1) .&. w1
        lowPositionBits = fromIntegral w2
        position = fromIntegral (shift highPositionBits bitsPerByte) + lowPositionBits

main = getArgs >>= \ args -> interact (map (toEnum . fromIntegral) . (if null args then compress else decompress) . map (fromIntegral . fromEnum) )