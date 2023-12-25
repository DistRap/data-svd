module Data.SVD.Pretty.Explore
  ( exploreRegister
  ) where

import Data.Bits (FiniteBits)
import Data.SVD.Types (Register(..), Field(..))
import Data.Word (Word8, Word32)
import Text.Printf (PrintfArg)

import qualified Data.Bits.Pretty
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
exploreRegister x addr reg = do
  putStrLn
    $ "Register " ++ regName reg
  putStrLn
    $ "- " ++ regDescription reg

  let addrW32 =
        Data.Bits.Pretty.showHex
          (fromIntegral addr :: Word32)
      addrWithOffset =
        Data.Bits.Pretty.showHex
          (fromIntegral (regAddressOffset reg) :: Word8)

  putStrLn
    $  "- Address "
    <> addrW32
    <> " (including offset "
    <> addrWithOffset
    <> ")"
  putStrLn ""

  case x of
    0 -> putStrLn "(Just zeros)"
    _ -> do

      putStrLn
        $ Data.Bits.Pretty.showDec x
      putStrLn
        $ Data.Bits.Pretty.showHex x
      putStrLn
        $ Data.Bits.Pretty.showBin x
      putStrLn
        $  "0b"
        <> Data.Bits.Pretty.showBinGroups 4 x

      putStrLn
        $ printSetFields
        $ Data.SVD.Util.getFieldValues
            x
            (regFields reg)

  putStrLn ""
  Data.SVD.Pretty.Box.renderFields
    $ Data.SVD.Util.getFieldValues
        x
        (regFields reg)

-- | Print currently set (non-zero) fields
printSetFields
  :: ( Show a
     , Eq a
     , Num a
     )
  => [(a, Field)]
  -> String
printSetFields =
    unlines
  . map printSetField
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

printSetField
  :: ( Show a
     , Eq a
     , Num a
     )
  => (a, Field)
  -> String
printSetField (_, f) | fieldBitWidth f == 1 =
  concat
    [ "Bit "
    , show (fieldBitOffset f)
    , " "
    , fieldName f
    ]
printSetField (v, f) | otherwise =
  concat
    [ "Bits ["
    , show (fieldBitOffset f)
    , ":"
    , show (fieldBitOffset f + fieldBitWidth f - 1)
    , "]"
    , " "
    , fieldName f
    , " value ", show v
    ]
