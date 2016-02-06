{-# LANGUAGE BinaryLiterals #-}
import qualified Data.ByteString.Lazy as B
import qualified Codec.Binary.UTF8.String as U
import Data.Bits
import Data.Char (ord, chr)
import Data.Word

main = do
	input <- B.getContents
	B.putStr . B.pack . (map fromIntegral) . (encode 0 0) . (map ord) . U.decode . B.unpack $ input
	where
		{-
		'0xxx xxxx' to represent '(p1)(p0)xxx xxxx' char
		'10xx xxxx' to set p0 to 'xx xxxx' (initially set to 0)
		'11xx xxxx' to set p1 to 'xx xxxx' (initially set to 0)
		-}
		-- ppN is for currently used prefix bits
		encode :: (Eq a, Num a, Bits a) => a -> a -> [a] -> [a]
		encode pp0 pp1 (c:cs) =
			( if pp1 == p1 then id else (p1' :) ) $
			( if pp0 == p0 then id else (p0' :) ) $
			b : encode p0 p1 cs
			where
				p0 = (c `shiftR` 7)     .&. 0b111111
				p1 = (c `shiftR` (7+6)) .&. 0b111111
				p0' = p0 .|. 0b10000000
				p1' = p1 .|. 0b11000000
				b = c .&. 0b1111111
				-- XXX values greater than 0b1111111111111111111 (0x7ffff)?
		encode _ _ [] = []
