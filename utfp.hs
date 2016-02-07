{-# LANGUAGE BinaryLiterals #-}
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as U
import Data.Bits
import Data.Char (ord, chr)
import Data.Word

main = do
	input <- B.getContents
	B.putStr . B.pack . (map fromIntegral) . (encode 0 0 0 0) . (map ord) . U.decode . B.unpack $ input
	where
		{-
		'0xxx xxxx' to represent '(p3)(p2)(p1)(p0)xxx xxxx' char
		'100x xxxx' to set p0 to 'x xxxx' (initially set to 0)
		'101x xxxx' to set p1 to 'x xxxx' (initially set to 0)
		'110x xxxx' to set p2 to 'x xxxx' (initially set to 0)
		'111x xxxx' to set p3 to 'x xxxx' (initially set to 0)

		This covers the range of 0x00..0x7ffffff
		-}
		-- ppN is for currently used prefix bits
		encode :: (Eq a, Num a, Bits a) => a -> a -> a -> a -> [a] -> [a]
		encode pp0 pp1 pp2 pp3 (c:cs) =
			( if pp3 == p3 then id else (p3' :) ) $
			( if pp2 == p2 then id else (p2' :) ) $
			( if pp1 == p1 then id else (p1' :) ) $
			( if pp0 == p0 then id else (p0' :) ) $
			b : encode p0 p1 p2 p3 cs
			where
				p0 = (c `shiftR` 7)         .&. 0b11111
				p1 = (c `shiftR` (7+5))     .&. 0b11111
				p2 = (c `shiftR` (7+5+5))   .&. 0b11111
				p3 = (c `shiftR` (7+5+5+5)) .&. 0b11111
				p0' = p0 .|. 0b10000000
				p1' = p1 .|. 0b10100000
				p2' = p2 .|. 0b11000000
				p3' = p3 .|. 0b11100000
				b = c .&. 0b1111111
		encode _ _ _ _ [] = []
