{-# LANGUAGE TypeApplications #-}
module DimSpec where

import Data.Default.Class
import Data.SVD
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Data.SVD.Dim" $ do
  describe "expandField" $ do
    it "no dim" $
      expandField def `shouldBe` mempty

    it "sampleField0" $
      expandField sampleField0 `shouldBe` resultField0

    it "sampleField1" $
      expandField sampleField1 `shouldBe` resultField1

  describe "expandRegister" $ do
    it "sampleReg0" $
      expandRegister sampleReg0 `shouldBe` resultReg0

    it "sampleReg1" $
      expandRegister sampleReg1 `shouldBe` resultReg1


sampleDim0 =
  Dimension
    { dimensionSize = 3
    , dimensionIndex = DimensionIndex_FromTo 0 2
    , dimensionIncrement = 0x2
    }

sampleField0 = def
  { fieldName = "FX%s"
  , fieldDimension = pure sampleDim0
  , fieldDescription = "Desc %s foo"
  , fieldBitOffset = 0x2
  }

resultField0 =
  [ def
      { fieldName = "FX0"
      , fieldDescription = "Desc 0 foo"
      , fieldBitOffset = 0x2
      }
  , def
      { fieldName = "FX1"
      , fieldDescription = "Desc 1 foo"
      , fieldBitOffset = 0x4
      }
  , def
      { fieldName = "FX2"
      , fieldDescription = "Desc 2 foo"
      , fieldBitOffset = 0x6
      }
  ]

sampleDim1 =
  Dimension
    { dimensionSize = 2
    , dimensionIndex = DimensionIndex_FromTo 1 2
    , dimensionIncrement = 0x2
    }

sampleField1 = def
  { fieldName = "FY%s"
  , fieldDimension = pure sampleDim1
  , fieldDescription = "Desc %s foo"
  , fieldBitOffset = 0x2
  }

resultField1 =
  [ def
      { fieldName = "FY1"
      , fieldDescription = "Desc 1 foo"
      , fieldBitOffset = 0x2
      }
  , def
      { fieldName = "FY2"
      , fieldDescription = "Desc 2 foo"
      , fieldBitOffset = 0x4
      }
  ]

-- * Register

sampleRegDim0 =
  Dimension
    { dimensionSize = 1
    , dimensionIndex = DimensionIndex_FromTo 1 1
    , dimensionIncrement = 0x4
    }

sampleReg0 = def
  { regName = "CCR%s"
  , regDimension = pure sampleRegDim0
  , regDescription = "Desc %s"
  , regAddressOffset = 0x34
  }

resultReg0 =
  [ def
      { regName = "CCR1"
      , regDescription = "Desc 1"
      , regAddressOffset = 0x34
      }
  ]

sampleRegDim1 =
  Dimension
    { dimensionSize = 2
    , dimensionIndex = DimensionIndex_List ["AB", "CD"]
    , dimensionIncrement = 0x4
    }

sampleReg1 = def
  { regName = "TEST%s"
  , regDimension = pure sampleRegDim1
  , regDescription = "Desc %s foo"
  , regAddressOffset = 0x34
  }

resultReg1 =
  [ def
      { regName = "TESTAB"
      , regDescription = "Desc AB foo"
      , regAddressOffset = 0x34
      }
  , def
      { regName = "TESTCD"
      , regDescription = "Desc CD foo"
      , regAddressOffset = 0x38
      }
  ]
