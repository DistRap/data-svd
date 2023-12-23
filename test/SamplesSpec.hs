{-# LANGUAGE LambdaCase #-}
module SamplesSpec where

import Control.Lens
import Data.Default.Class
import Data.Either
import Data.SVD
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Golden (defaultGolden)
import Text.PrettyPrint.ANSI.Leijen (plain)

spec :: Spec
spec = describe "Golden" $ do
  describe "stm32f405.svd" $ do
    it "isRight" $ do
      res <- parseSVD "./test/samples/stm32f405.svd"
      res `shouldSatisfy` isRight

    it "has right number of peripherals" $ do
      res <-
        fmap
        (length . view peripherals)
        <$> parseSVD "./test/samples/stm32f405.svd"
      res `shouldBe` (Right 85)

    it "matches golden pretty printed device" $ do
      pretty <-
        parseSVD "./test/samples/stm32f405.svd"
        >>= \case
          Left e -> error "Failed parsing"
          Right x ->
            pure
            $ displayPretty
            $ plain
            $ ppDevice x
      pure $ defaultGolden "stm32f405" pretty
