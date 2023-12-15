module Data.SVD.Diff where

import Data.Algorithm.Diff (Diff, PolyDiff(..))
import qualified Data.Algorithm.Diff
import qualified Data.List
import qualified Data.Maybe
import qualified Safe

import Data.SVD.Types
  ( Device
  , Peripheral
  , Field
  , devicePeripherals
  , fieldBitOffset
  , fieldBitWidth
  , fieldName
  , fieldNames
  , periphName
  , periphRegisters
  , registerNames
  , regFields
  , regName
  )

diffPeriphNames
  :: Device
  -> Device
  -> [Diff String]
diffPeriphNames dev1 dev2 =
  Data.Algorithm.Diff.getDiff
    (Data.List.sort $ map periphName $ devicePeripherals dev1)
    (Data.List.sort $ map periphName $ devicePeripherals dev2)

diffRegisterNames
  :: String
  -> Device
  -> Device
  -> [Diff String]
diffRegisterNames pName dev1 dev2 =
  Data.Algorithm.Diff.getDiff
    (Data.List.sort $ registerNames pName dev1)
    (Data.List.sort $ registerNames pName dev2)

regNames :: Peripheral -> [String]
regNames = map regName . periphRegisters

diffRegNames :: Peripheral -> Peripheral -> [Diff String]
diffRegNames = diff regNames

regNameFields :: String -> Peripheral -> [Field]
regNameFields rName =
    regFields
  . Safe.headNote "regNameFields"
  . filter((==rName) . regName)
  . periphRegisters

diff
  :: Ord a
  => (t -> [a])
  -> t
  -> t
  -> [Diff a]
diff fn x y =
  Data.Algorithm.Diff.getDiff
    (Data.List.sort $ fn x)
    (Data.List.sort $ fn y)

diffFieldNames
  :: String
  -> String
  -> Device
  -> Device
  -> [Diff String]
diffFieldNames pName regName' dev1 dev2 =
  Data.Algorithm.Diff.getDiff
    (Data.List.sort $ fieldNames regName' pName dev1)
    (Data.List.sort $ fieldNames regName' pName dev2)

diffFields
  :: [Field]
  -> [Field]
  -> [PolyDiff Field Field]
diffFields as bs =
  Data.Algorithm.Diff.getDiffBy
    (\x y ->
      cmps fieldName x y
      && cmps fieldBitWidth x y
      && cmps fieldBitOffset x y
    )
  (Data.List.sortOn fieldBitOffset as)
  (Data.List.sortOn fieldBitOffset bs)
  where
    cmps fn a b = fn a == fn b

diffDistance :: [PolyDiff a b] -> Int
diffDistance x =
  sum $ map go x
  where
    go (Both _ _) = 0
    go (First  _) = 1
    go (Second _) = 1

getBoths :: [PolyDiff a b] -> [a]
getBoths = Data.Maybe.catMaybes . map ex
  where
    ex (Both x _) = Just x
    ex _ = Nothing
