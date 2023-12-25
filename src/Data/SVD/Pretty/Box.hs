{-# LANGUAGE RecordWildCards #-}

module Data.SVD.Pretty.Box
  ( renderFields
  ) where

import Data.Bits (Bits())
import Data.SVD.Types (Field(..))
import Data.Word (Word8, Word16, Word32, Word64)
import Text.PrettyPrint.Boxes (Box, (//))
import qualified Text.PrettyPrint.Boxes

import qualified Data.List
import qualified Data.Bits.Pretty

-- | Render fields as table using boxes
-- If table would be too wide split it into two tables
renderFields
  :: ( Bits a
     , Num a
     , Show a
     , Integral a)
  => [(a, Field)]
  -> IO ()
renderFields fs | headerSize >= 80 = do
  putStrLn "MSB"
  putStrLn
    $ Text.PrettyPrint.Boxes.render
    $ table
    $ remap
    $ takeBits 16 fs
  putStrLn "LSB"
  putStrLn
    $ Text.PrettyPrint.Boxes.render
    $ table
    $ remap
    $ dropBits 16 fs
  where
    headerSize =
      sum
      $ map
          (length . showField . snd)
          fs

renderFields fs | otherwise =
    putStrLn
  . Text.PrettyPrint.Boxes.render
  . table
  . remap
  $ fs

table :: [[String]] -> Box
table rows =
  hSepDeco
  Text.PrettyPrint.Boxes.<>
     Text.PrettyPrint.Boxes.punctuateH
       Text.PrettyPrint.Boxes.top
       hSepDeco
       (map fmtColumn cols)
  Text.PrettyPrint.Boxes.<> hSepDeco
    where
      cols = Data.List.transpose rows
      nrows = length rows
      hSepDeco =
       Text.PrettyPrint.Boxes.vcat
         Text.PrettyPrint.Boxes.left
         $ map
             Text.PrettyPrint.Boxes.char
             (
               "+"
               <>
               (concat $ replicate nrows "|+")
             )

fmtColumn :: [String] -> Box
fmtColumn items =
     vSepDeco
  // Text.PrettyPrint.Boxes.punctuateV
       Text.PrettyPrint.Boxes.center2
       vSepDeco
       (map
          Text.PrettyPrint.Boxes.text
          items
        )
  // vSepDeco
  where width = maximum $ map length items
        vSepDeco =
          Text.PrettyPrint.Boxes.text
          $ replicate width '-'

remap
  :: ( Integral x
     , Show x
     )
  => [(x, Field)]
  -> [[String]]
remap fs =
  [ map
      (showField . snd)
      fs
  , map
      (\(v, f) -> hexFieldVal f v)
      fs
  ]

takeBits
  :: Int
  -> [(a, Field)]
  -> [(a, Field)]
takeBits 0 _ = []
takeBits x (y@(_, f):fs) | x >= fieldBitWidth f = y : (takeBits (x - fieldBitWidth f) fs)
takeBits x (y@(_, f):_fs) | x <  fieldBitWidth f = [splitField x y]
  where
    splitField x' (v, f') =
      ( v
      , f
          { fieldBitWidth = x'
          , fieldBitOffset = fieldBitOffset f' + (fieldBitWidth f' - x')
          }
      )
takeBits _ _ = []

dropBits
  :: Int
  -> [(a, Field)]
  -> [(a, Field)]
dropBits 0 fs = fs
dropBits x ((_, f):fs) | x >= fieldBitWidth f = dropBits (x - fieldBitWidth f) fs
dropBits x (y@(_, f):fs) | x <  fieldBitWidth f = (splitField x y):fs
  where
    splitField x' (v, f') =
      ( v
      , f { fieldBitWidth = fieldBitWidth f' - x' }
      )
dropBits _ _ = []

-- | Show `Field` with its range, e.g BRR[15:0] (16 bit wide)
showField :: Field -> String
showField f@Field{..} | fieldReserved =
     "◦"
  <> fieldRange f
showField f@Field{..} | otherwise =
     fieldName
  <> fieldRange f

-- | Datasheeeet like
fieldRange :: Field -> String
fieldRange Field{..} | fieldBitWidth == 1 = ""
fieldRange Field{..} | otherwise =
  concat
    [ "["
    , show $ fieldBitWidth - 1
    , ":0]"
    ]

-- | Format field value in hex, padded according to `fieldBitWidth`
hexFieldVal :: (Integral x, Show x) => Field -> x -> String
hexFieldVal _ 0 = "0"
hexFieldVal f x | fieldBitWidth f ==  1 = showBit x
  where
    showBit 0 = "0"
    showBit 1 = "1"
    showBit y = error $ "Not a bit: " ++ show y
hexFieldVal f x | fieldBitWidth f <=  8 =
  Data.Bits.Pretty.showHex (fromIntegral x :: Word8)
hexFieldVal f x | fieldBitWidth f <= 16 =
  Data.Bits.Pretty.showHex (fromIntegral x :: Word16)
hexFieldVal f x | fieldBitWidth f <= 32 =
  Data.Bits.Pretty.showHex (fromIntegral x :: Word32)
hexFieldVal _ x | otherwise =
  Data.Bits.Pretty.showHex (fromIntegral x :: Word64)
