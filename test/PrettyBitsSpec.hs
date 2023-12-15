{-# LANGUAGE TypeApplications #-}
module PrettyBitsSpec where

import Data.Word (Word8, Word16, Word32)
import Data.Bits.Pretty
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Data.Bits.Pretty" $ do
  it "showBin Word8" $ showBin @Word8 123 `shouldBe` "0b01111011"
  it "showBin Word16" $ showBin @Word16 123 `shouldBe` "0b0000000001111011"
  it "showDec Word8" $ showDec @Word8 123 `shouldBe` "123"
  it "showDec Word16" $ showDec @Word16 123 `shouldBe` "00123"
  it "showHex Word8" $ showHex @Word8 123 `shouldBe` "0x7B"
  it "showHex Word16" $ showHex @Word16 123 `shouldBe` "0x007B"
  it "showHex8" $ showHex8 123 `shouldBe` "0x7B"
  it "showHex16" $ showHex16 123 `shouldBe` "0x007B"
  it "showHex32" $ showHex32 123 `shouldBe` "0x0000007B"

  it "formatHex Word8" $ formatHex @Word8 123 `shouldBe` "0x7b"
  it "formatHex Word16" $ formatHex @Word16 123 `shouldBe` "0x7b"

  it "showBinGroups 4 Word8"
    $ showBinGroups @Word8 4 123
    `shouldBe` "0111 1011"
  it "showBinGroups 4 Word16"
    $ showBinGroups @Word16 4 maxBound
    `shouldBe` "1111 1111 1111 1111"
  it "showBinGroups 8 Word16"
    $ showBinGroups @Word16 8 maxBound
    `shouldBe` "11111111 11111111"
  it "showBinGroups 4 Word32"
    $ showBinGroups @Word32 4 maxBound
    `shouldBe` "1111 1111 1111 1111 1111 1111 1111 1111"
