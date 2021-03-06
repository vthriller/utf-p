{-# LANGUAGE BinaryLiterals #-}
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as U
import Data.Bits
import Data.Char (ord, chr)
import Data.Word
import System.Environment (getArgs)

main :: IO ()
main = do
	input <- B.getContents
	args <- getArgs
	if "-d" `elem` args
	then B.putStr . B.pack . U.encode . (map $ chr . fromIntegral) . (decode 0 0 0) . (map fromIntegral :: [Word8] -> [Word64]) . B.unpack $ input
	else B.putStr . B.pack . (map fromIntegral) . (encode 0 0 0) . (map ord) . U.decode . B.unpack $ input

{-
'0xxx xxxx' to represent '(p2)(p1)(p0)xxx xxxx' char
'10xx xxxx' to set p0 to 'xx xxxx' (initially set to 0)
'110x xxxx' to set p1 to 'x xxxx' (initially set to 0)
'111x xxxx' to set p2 to 'x xxxx' (initially set to 0)

This covers the range of 0x00..0x7fffff
-}

-- ppN is for currently used prefix bits
encode :: (Eq a, Num a, Bits a) => a -> a -> a -> [a] -> [a]
encode pp0 pp1 pp2 (c:cs) =
	( if pp2 == p2 then id else (p2' :) ) $
	( if pp1 == p1 then id else (p1' :) ) $
	( if pp0 == p0 then id else (p0' :) ) $
	b : encode p0 p1 p2 cs
	where
		p0 = (c `shiftR` 7)         .&. 0b111111
		p1 = (c `shiftR` (7+6))     .&. 0b11111
		p2 = (c `shiftR` (7+6+5))   .&. 0b11111
		p0' = p0 .|. 0b10000000
		p1' = p1 .|. 0b11000000
		p2' = p2 .|. 0b11100000
		b = c .&. 0b1111111
encode _ _ _ [] = []

decode :: (Eq a, Num a, Bits a, Show a) => a -> a -> a -> [a] -> [a]
decode p0 p1 p2 (c:cs)
	| c .&. 0b10000000 == 0b00000000 =
		(c .|. p') : decode p0 p1 p2 cs
	| c .&. 0b11000000 == 0b10000000 =
		decode (c .&. 0b00111111) p1 p2 cs
	| c .&. 0b11100000 == 0b11000000 =
		decode p0 (c .&. 0b00011111) p2 cs
	| c .&. 0b11100000 == 0b11100000 =
		decode p0 p1 (c .&. 0b00011111) cs
	-- non-exhaustive matches, y'say?
	| otherwise = error $ "Impossible byte: " ++ (show c)
	where
		p0' = p0 `shift` 7
		p1' = p1 `shift` (7+6)
		p2' = p2 `shift` (7+6+5)
		p' = p0' .|. p1' .|. p2'
decode _ _ _ [] = []
