import Math.Base62 (b62UnitEncode, b62UnitDecode, b62Encode, b62Decode)

import Test.Hspec
import Test.HUnit
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "Base62 test suite" $ do
    it "expected encode all unit in base10 to base62" $ do
      (b62UnitEncode  0) `shouldBe` '0'
      (b62UnitEncode  1) `shouldBe` '1'
      (b62UnitEncode  2) `shouldBe` '2'
      (b62UnitEncode  3) `shouldBe` '3'
      (b62UnitEncode  4) `shouldBe` '4'
      (b62UnitEncode  5) `shouldBe` '5'
      (b62UnitEncode  6) `shouldBe` '6'
      (b62UnitEncode  7) `shouldBe` '7'
      (b62UnitEncode  8) `shouldBe` '8'
      (b62UnitEncode  9) `shouldBe` '9'
      (b62UnitEncode 10) `shouldBe` 'A'
      (b62UnitEncode 11) `shouldBe` 'B'
      (b62UnitEncode 12) `shouldBe` 'C'
      (b62UnitEncode 13) `shouldBe` 'D'
      (b62UnitEncode 14) `shouldBe` 'E'
      (b62UnitEncode 15) `shouldBe` 'F'
      (b62UnitEncode 16) `shouldBe` 'G'
      (b62UnitEncode 17) `shouldBe` 'H'
      (b62UnitEncode 18) `shouldBe` 'I'
      (b62UnitEncode 19) `shouldBe` 'J'
      (b62UnitEncode 20) `shouldBe` 'K'
      (b62UnitEncode 21) `shouldBe` 'L'
      (b62UnitEncode 22) `shouldBe` 'M'
      (b62UnitEncode 23) `shouldBe` 'N'
      (b62UnitEncode 24) `shouldBe` 'O'
      (b62UnitEncode 25) `shouldBe` 'P'
      (b62UnitEncode 26) `shouldBe` 'Q'
      (b62UnitEncode 27) `shouldBe` 'R'
      (b62UnitEncode 28) `shouldBe` 'S'
      (b62UnitEncode 29) `shouldBe` 'T'
      (b62UnitEncode 30) `shouldBe` 'U'
      (b62UnitEncode 31) `shouldBe` 'V'
      (b62UnitEncode 32) `shouldBe` 'W'
      (b62UnitEncode 33) `shouldBe` 'X'
      (b62UnitEncode 34) `shouldBe` 'Y'
      (b62UnitEncode 35) `shouldBe` 'Z'
      (b62UnitEncode 36) `shouldBe` 'a'
      (b62UnitEncode 37) `shouldBe` 'b'
      (b62UnitEncode 38) `shouldBe` 'c'
      (b62UnitEncode 39) `shouldBe` 'd'
      (b62UnitEncode 40) `shouldBe` 'e'
      (b62UnitEncode 41) `shouldBe` 'f'
      (b62UnitEncode 42) `shouldBe` 'g'
      (b62UnitEncode 43) `shouldBe` 'h'
      (b62UnitEncode 44) `shouldBe` 'i'
      (b62UnitEncode 45) `shouldBe` 'j'
      (b62UnitEncode 46) `shouldBe` 'k'
      (b62UnitEncode 47) `shouldBe` 'l'
      (b62UnitEncode 48) `shouldBe` 'm'
      (b62UnitEncode 49) `shouldBe` 'n'
      (b62UnitEncode 50) `shouldBe` 'o'
      (b62UnitEncode 51) `shouldBe` 'p'
      (b62UnitEncode 52) `shouldBe` 'q'
      (b62UnitEncode 53) `shouldBe` 'r'
      (b62UnitEncode 54) `shouldBe` 's'
      (b62UnitEncode 55) `shouldBe` 't'
      (b62UnitEncode 56) `shouldBe` 'u'
      (b62UnitEncode 57) `shouldBe` 'v'
      (b62UnitEncode 58) `shouldBe` 'w'
      (b62UnitEncode 59) `shouldBe` 'x'
      (b62UnitEncode 60) `shouldBe` 'y'
      (b62UnitEncode 61) `shouldBe` 'z'
    it "expected decode all unit in base62 to base10" $ do
      (b62UnitDecode '0') `shouldBe` 0
      (b62UnitDecode '1') `shouldBe` 1
      (b62UnitDecode '2') `shouldBe` 2
      (b62UnitDecode '3') `shouldBe` 3
      (b62UnitDecode '4') `shouldBe` 4
      (b62UnitDecode '5') `shouldBe` 5
      (b62UnitDecode '6') `shouldBe` 6
      (b62UnitDecode '7') `shouldBe` 7
      (b62UnitDecode '8') `shouldBe` 8
      (b62UnitDecode '9') `shouldBe` 9
      (b62UnitDecode 'A') `shouldBe` 10
      (b62UnitDecode 'B') `shouldBe` 11
      (b62UnitDecode 'C') `shouldBe` 12
      (b62UnitDecode 'D') `shouldBe` 13
      (b62UnitDecode 'E') `shouldBe` 14
      (b62UnitDecode 'F') `shouldBe` 15
      (b62UnitDecode 'G') `shouldBe` 16
      (b62UnitDecode 'H') `shouldBe` 17
      (b62UnitDecode 'I') `shouldBe` 18
      (b62UnitDecode 'J') `shouldBe` 19
      (b62UnitDecode 'K') `shouldBe` 20
      (b62UnitDecode 'L') `shouldBe` 21
      (b62UnitDecode 'M') `shouldBe` 22
      (b62UnitDecode 'N') `shouldBe` 23
      (b62UnitDecode 'O') `shouldBe` 24
      (b62UnitDecode 'P') `shouldBe` 25
      (b62UnitDecode 'Q') `shouldBe` 26
      (b62UnitDecode 'R') `shouldBe` 27
      (b62UnitDecode 'S') `shouldBe` 28
      (b62UnitDecode 'T') `shouldBe` 29
      (b62UnitDecode 'U') `shouldBe` 30
      (b62UnitDecode 'V') `shouldBe` 31
      (b62UnitDecode 'W') `shouldBe` 32
      (b62UnitDecode 'X') `shouldBe` 33
      (b62UnitDecode 'Y') `shouldBe` 34
      (b62UnitDecode 'Z') `shouldBe` 35
      (b62UnitDecode 'a') `shouldBe` 36
      (b62UnitDecode 'b') `shouldBe` 37
      (b62UnitDecode 'c') `shouldBe` 38
      (b62UnitDecode 'd') `shouldBe` 39
      (b62UnitDecode 'e') `shouldBe` 40
      (b62UnitDecode 'f') `shouldBe` 41
      (b62UnitDecode 'g') `shouldBe` 42
      (b62UnitDecode 'h') `shouldBe` 43
      (b62UnitDecode 'i') `shouldBe` 44
      (b62UnitDecode 'j') `shouldBe` 45
      (b62UnitDecode 'k') `shouldBe` 46
      (b62UnitDecode 'l') `shouldBe` 47
      (b62UnitDecode 'm') `shouldBe` 48
      (b62UnitDecode 'n') `shouldBe` 49
      (b62UnitDecode 'o') `shouldBe` 50
      (b62UnitDecode 'p') `shouldBe` 51
      (b62UnitDecode 'q') `shouldBe` 52
      (b62UnitDecode 'r') `shouldBe` 53
      (b62UnitDecode 's') `shouldBe` 54
      (b62UnitDecode 't') `shouldBe` 55
      (b62UnitDecode 'u') `shouldBe` 56
      (b62UnitDecode 'v') `shouldBe` 57
      (b62UnitDecode 'w') `shouldBe` 58
      (b62UnitDecode 'x') `shouldBe` 59
      (b62UnitDecode 'y') `shouldBe` 60
      (b62UnitDecode 'z') `shouldBe` 61
    it "expected encode of number '21700' to base62 equal to '5e0'" $ do
      (b62Encode 21700) `shouldBe` "5e0"
    it "expected encode of long number '1234567890' to base62 equal to '1LY7VK'" $ do
      (b62Encode 1234567890) `shouldBe` "1LY7VK"
    it "expected decode of number '5e0' to base10 equal to '21700'" $ do
      (b62Decode "5e0") `shouldBe` 21700
    it "expected decode of long number 1LY7VK to base10 equal to 1234567890" $ do
      (b62Decode "1LY7VK") `shouldBe` 1234567890
