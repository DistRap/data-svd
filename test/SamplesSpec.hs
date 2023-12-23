{-# LANGUAGE LambdaCase #-}
module SamplesSpec where

import Control.Lens
import Data.Default.Class
import Data.Either
import Data.SVD
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Golden (defaultGolden)
import Prettyprinter (unAnnotate)

import qualified Data.List

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
            $ unAnnotate
            $ ppDevice x
      pure $ defaultGolden "stm32f405" pretty

    it "matches golden pretty printed device isrs" $ do
      pretty <-
        parseSVD "./test/samples/stm32f405.svd"
        >>= \case
          Left e -> error "Failed parsing"
          Right x ->
            pure
            $ displayPretty
            $ unAnnotate
            $ ppList ppISR
            $ Data.List.sortOn
                interruptValue
            $ fillMissingInterrupts
            $ Data.List.nubBy
                (\x y -> interruptValue x == interruptValue y)
            $ concatMap
                periphInterrupts
                (devicePeripherals x)
      pure $ defaultGolden "stm32f405-isrs" pretty

    it "matches golden pretty printed device memory map" $ do
      pretty <-
        parseSVD "./test/samples/stm32f405.svd"
        >>= \case
          Left e -> error "Failed parsing"
          Right x ->
            pure
            $ displayPretty
            $ unAnnotate
            $ ppList ppMem
            $ getDevMemMap x
      pure $ defaultGolden "stm32f405-memmap" pretty
