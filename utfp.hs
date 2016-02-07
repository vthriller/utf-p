{-# LANGUAGE BinaryLiterals #-}
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as U
import Data.Bits
import Data.Char (ord, chr)
import Data.Word

main = do
	input <- B.getContents
	B.putStr . B.pack . (map fromIntegral) . (encode 1 0 0) . (map ord) . U.decode . B.unpack $ input
	where
		{-
		'00xx xxxx' to represent 'xx xxxx' char
			used to avoid prefix setters for commonly used whitespace characters, digits, and punctuation
		'01xx xxxx' to represent '(p2)(p1)(p0)xx xxxx' char
		'10xx xxxx' to set p0 to 'xx xxxx' (initially set to 1)
			this initial value makes ASCII latin renderable in absence of '1000 0001' prefix setter
		'110x xxxx' to set p1 to 'x xxxx' (initially set to 0)
		'111x xxxx' to set p2 to 'x xxxx' (initially set to 0)

		This covers the range of 0x00..0x3fffff,
		which happens to cover all valid Unicode code points (U+0000 to U+10FFFF)
		-}
		-- ppN is for currently used prefix bits
		encode :: (Eq a, Ord a, Num a, Bits a) => a -> a -> a -> [a] -> [a]
		encode pp0 pp1 pp2 (c:cs) =
			if c < 0b1000000
			then c : encode pp0 pp1 pp2 cs
			else
			( if pp2 == p2 then id else (p2' :) ) $
			( if pp1 == p1 then id else (p1' :) ) $
			( if pp0 == p0 then id else (p0' :) ) $
			b : encode p0 p1 p2 cs
			where
				p0 = (c `shiftR` 6)         .&. 0b111111
				p1 = (c `shiftR` (6+6))     .&. 0b11111
				p2 = (c `shiftR` (6+6+5))   .&. 0b11111
				p0' = p0 .|. 0b10000000
				p1' = p1 .|. 0b11000000
				p2' = p2 .|. 0b11100000
				b = (c .&. 0b111111) .|. 0b01000000
		encode _ _ _ [] = []
