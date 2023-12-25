{-# LANGUAGE OverloadedStrings #-}
module Data.SVD.Pretty.Explore
  ( exploreRegister
  ) where

import Data.Bits (FiniteBits)
import Data.SVD.Types (Register(..), Field(..))
import Data.Word (Word8, Word16, Word32)
import Prettyprinter
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), bold, color)
import Text.Printf (PrintfArg)

import qualified Data.Bits.Pretty
import qualified Data.SVD.Pretty
import qualified Data.SVD.Pretty.Box
import qualified Data.SVD.Util

exploreRegister
  :: ( PrintfArg a
     , FiniteBits a
     , Show a
     , Integral a
     )
  => a
  -> Int
  -> Register
  -> IO ()
exploreRegister x addr reg =
    putStrLn
  $ Data.SVD.Pretty.displayPretty
  $ exploreRegister' x addr reg

exploreRegister'
  :: ( PrintfArg a
     , FiniteBits a
     , Show a
     , Integral a
     )
  => a
  -> Int
  -> Register
  -> Doc AnsiStyle
exploreRegister' x addr reg =
      "Register"
  <+> annotate
        (bold <> color Red)
        (pretty $ regName reg)
  <>  line
  <>  "-"
  <+> annotate
        (color Magenta)
        (pretty (regDescription reg))
  <>  line
  <>  "- Address"
  <+> annotate
        (color Blue)
        (pretty
          (Data.Bits.Pretty.showHex
            (fromIntegral addr :: Word32)
          )
        )
  <+> parens
        (  "including offset "
        <> annotate
             (color Blue)
             (pretty
               (Data.Bits.Pretty.showHex
                 (fromIntegral (regAddressOffset reg) :: Word8)
               )
             )
        )
  <> line
  <> line
  <> case x of
    0 -> "(Just zeros)"
    _ ->
      vsep
      [ annotate
          (color Green)
          (    "DEC"
           <+> pretty
                 (Data.Bits.Pretty.showDec x)
          )
      , annotate
          (color Cyan)
          (    "HEX"
           <+> pretty
                 (Data.Bits.Pretty.showHex x)
          )
      , annotate
          (color White)
          (   "BIN"
          <+> pretty
                (Data.Bits.Pretty.showBin x)
          )
      , annotate
          (color Yellow)
          (   "BIN"
          <+> "0b"
          <> pretty
               (Data.Bits.Pretty.showBinGroups 4 x)
          )
      , prettySetFields
          (Data.SVD.Util.getFieldValues
             x
             (regFields reg)
          )
      ]
  <> line
  <> line
  <> pretty
       (Data.SVD.Pretty.Box.renderFields
          $ Data.SVD.Util.getFieldValues
              x
              (regFields reg)
       )

-- | Print currently set (non-zero) fields
prettySetFields
  :: ( Show a
     , Eq a
     , Num a
     , FiniteBits a
     , PrintfArg a
     , Integral a
     )
  => [(a, Field)]
  -> Doc AnsiStyle
prettySetFields =
    vsep
  . map prettySetField
  . filterSet
  where
    -- | Filter fields with non zero value
    filterSet
      :: ( Eq a
         , Num a
         )
      => [(a, Field)]
      -> [(a, Field)]
    filterSet = filter ((/= 0) . fst)

prettySetField
  :: ( Show a
     , Eq a
     , Num a
     , FiniteBits a
     , PrintfArg a
     , Integral a
     )
  => (a, Field)
  -> Doc AnsiStyle
prettySetField (_, f) | fieldBitWidth f == 1 =
  hcat
    [ "Bit "
    , pretty (fieldBitOffset f)
    , " "
    , annotate
        (color Cyan)
        (pretty $ fieldName f)
    ]
prettySetField (v, f) | otherwise =
  hcat
    [ "Bits ["
    , pretty (fieldBitOffset f)
    , ":"
    , pretty (fieldBitOffset f + fieldBitWidth f - 1)
    , "]"
    , " "
    , annotate
        (color Cyan)
        (pretty $ fieldName f)
    , " value "
    , annotate
        (color Magenta)
        (pretty $ showFittingSize v)
    ]
  where
    showFittingSize x | fromIntegral x <= (maxBound :: Word8) =
      Data.Bits.Pretty.showHex8 (fromIntegral x)
    showFittingSize x | fromIntegral x <= (maxBound :: Word16) =
      Data.Bits.Pretty.showHex16 (fromIntegral x)
    showFittingSize x | fromIntegral x <= (maxBound :: Word32) =
      Data.Bits.Pretty.showHex32 (fromIntegral x)
    showFittingSize x | otherwise =
      Data.Bits.Pretty.showHex x
