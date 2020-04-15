module Math.Base62 (
  base62,
  b62UnitEncode,
  b62UnitDecode,
  b62UnitIncrement,
  b62UnitDecrement,
  b62Decode,
  b62Encode,
) where

import Data.List (find)

base62 :: [(Char, Int)]
base62 = [('0', 0),('1', 1),('2', 2),('3', 3),('4', 4),('5', 5),('6', 6),('7', 7),('8', 8),('9', 9),('A', 10),('B', 11),('C', 12),('D', 13),('E', 14),('F', 15),('G', 16),('H', 17),('I', 18),('J', 19),('K', 20),('L', 21),('M', 22),('N', 23),('O', 24),('P', 25),('Q', 26),('R', 27),('S', 28),('T', 29),('U', 30),('V', 31),('W', 32),('X', 33),('Y', 34),('Z', 35),('a', 36),('b', 37),('c', 38),('d', 39),('e', 40),('f', 41),('g', 42),('h', 43),('i', 44),('j', 45),('k', 46),('l', 47),('m', 48),('n', 49),('o', 50),('p', 51),('q', 52),('r', 53),('s', 54),('t', 55),('u', 56),('v', 57),('w', 58),('x', 59),('y', 60),('z', 61)]


b62UnitDecode :: Char -> Int
b62UnitDecode inputC = case find (\u -> fst u == inputC) base62 of
  Just n -> snd n
  Nothing -> 0

b62UnitEncode:: Int -> Char
b62UnitEncode inputI
  | inputI <= 62 && inputI >= 0 = fst (base62 !! inputI)

b62UnitIncrement :: Char -> Char
b62UnitIncrement inputC = b62UnitEncode (b62UnitDecode inputC + 1)

b62UnitDecrement :: Char -> Char
b62UnitDecrement inputC = b62UnitEncode (b62UnitDecode inputC - 1)

_b62Encode :: Int -> String
_b62Encode inputI
  | inputI == 0 = ""
  | otherwise = b62UnitEncode (inputI `mod` 62) : _b62Encode (inputI `div` 62)

b62Encode :: Int -> String
b62Encode xc = reverse $ _b62Encode(xc)

b62Decode :: String -> Int
b62Decode inputS
  | inputS == "" = 0
  | otherwise = b62UnitDecode(head inputS) * 62 ^ (length(inputS)-1) + b62Decode(tail inputS)

