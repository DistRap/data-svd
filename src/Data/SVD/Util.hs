{-# LANGUAGE RecordWildCards #-}

module Data.SVD.Util
  ( addReservedFields
  , procFields
  , continuityCheck
  , checkDeviceRegisterContinuity
  , mapPeriphs
  , mapRegs
  , mapFields
  , mapDevFields
  , getPeriphByGroup
  , getPeriph
  , getPeriphMay
  , getPeriphRegMay
  , getPeriphFollow
  , getPeriphRegs
  , getPeriphReg
  , getPeriphRegAddr
  , getPeriphRegFields
  , getRegFields
  , getFieldVal
  , getFieldValues
  , anyReservedSet
  , getDevMemMap
  , registerNames
  , fieldNames
  -- * Sorting
  , sortDeviceByAddresses
  , sortDeviceByNames
  -- * Interrupts
  , fillMissingInterrupts
  ) where

import Control.Lens ((^.), over, set, view)
import Control.Monad (liftM2)
import Data.Bits (Bits, shiftR, (.&.))
import Data.SVD.Lens
import Data.SVD.Types

import qualified Data.Char
import qualified Data.Bits.Pretty
import qualified Data.Either
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Set
import qualified Safe

-- | Find holes in registers and create corresponding reserved fields for these
--
-- First finds missing bits and then merges them to single reserved field
procFields :: Register -> [Field]
procFields Register{..} =
    dataIfSingleReserved
  $ reverse
  $ sortByOffset (regFields ++ missingAsReserved)
  where
    missingAsReserved =
      mkReserved
      $ conts
      $ Data.Set.toList missing

    mkReserved =
      map
        $ \(offset', width') ->
          Field
            { fieldName = "_"
            , fieldDescription = "(Reserved)"
            , fieldDimension = Nothing
            , fieldBitOffset = offset'
            , fieldBitWidth = width'
            , fieldReserved = True
            , fieldRegType = Nothing
            }

    conts x = case cont x of
      [] -> []
      s -> (head s, length s) : conts (drop (length s) x)

    -- find longest increasing sequence
    cont :: (Eq a, Num a) => [a] -> [a]
    cont (x:y:xs) | x + 1 == y = x : cont (y:xs)
    cont (x:_)  = [x]
    cont [] = []

    missing = allRegs `Data.Set.difference` existing

    allRegs = Data.Set.fromList [0..(regSize - 1)]

    existing =
      Data.Set.fromList
      $ flip concatMap (sortByOffset regFields)
      $ \Field{..} -> [fieldBitOffset .. (fieldBitOffset + fieldBitWidth - 1)]

    sortByOffset = Data.List.sortOn fieldBitOffset

    -- this handles a case when there are no fields and code above
    -- creates a single full-sized reserved field
    -- which we turn into non-reserved "data" field
    dataIfSingleReserved [f] | fieldReserved f =
      [ f {
            fieldName = "DATA"
          , fieldReserved = False
          }
      ]
    dataIfSingleReserved fs = fs

-- | Fill in reserved fields for whole @Device@
addReservedFields :: Device -> Device
addReservedFields =
  over
    (peripherals . traverse . registers . traverse)
    procRegister
  where
    procRegister r = set fields (procFields r) r

-- | Walk processed register fields top to bottom
-- checking that the register is exactly n bits long
continuityCheck :: Register -> Bool
continuityCheck Register{..} = go regFields regSize
  where
  go [] 0 = True
  go (x:xs) remainingBits
    | fieldBitOffset x + fieldBitWidth x == remainingBits
    = go xs (remainingBits - fieldBitWidth x)
  go _ _ = False

-- | Walk processed register fields top to bottom
-- checking that the register is exactly n bits long
continuityCheckReg
  :: Device
  -> Peripheral
  -> Register
  -> Either String Register

-- Some ignores
-- TIM5.CNT is 32 bit but has an aliased UIFCPY field
continuityCheckReg d p r
  | d ^. name `elem` [ "STM32F730", "STM32F745", "STM32F750", "STM32F765"
                     , "STM32F7x2", "STM32F7x3", "STM32F7x6", "STM32F7x7", "STM32F7x9" ]
  && p ^. name == "TIM5" && r ^. name == "CNT" = pure r
-- G4 TIM2.CCR5, might be a bug in stm32-rs
continuityCheckReg d p r
  | d ^. name `elem` [ "STM32G431xx", "STM32G441xx", "STM32G471xx", "STM32G473xx"
                     , "STM32G474xx", "STM32G483xx", "STM32G484xx", "STM32G491xx", "STM32G4A1xx" ]
  && p ^. name == "TIM2" && r ^. name == "CCR5" = pure r
continuityCheckReg d p r
  | d ^. name == "STM32H73x"
  && p ^. name == "CRYP" && r ^. name == "K2LR" = pure r
continuityCheckReg d p r
  | (d ^. name == "STM32L0x2" || d ^. name == "STM32L0x3")
  && p ^.name == "PWR" && r ^. name == "CR" = pure r
continuityCheckReg d p r
  | d ^. name == "STM32L4P5"
  && p ^.name == "TIM15" && r ^. name == "SR" = pure r
continuityCheckReg d p r
  | d ^. name == "STM32L4P5"
  && p ^.name == "SAI1" && r ^. name == "CR1" = pure r
continuityCheckReg d p r
  | d ^. name == "STM32L4P5"
  && p ^.name == "FLASH" && r ^. name == "ECCR" = pure r
continuityCheckReg d p r
  | d ^. name == "STM32WB55"
  && p ^.name == "TIM2" && r ^. name == "CNT" = pure r
continuityCheckReg d p r =
  go
    ( reverse
    $ Data.List.sortOn
        (view bitOffset)
        (r ^. fields)
    )
    (r ^. size)
  where
  go [] 0 = pure r

  go (x:xs) remainingBits
    | x ^. bitOffset + x ^. bitWidth == remainingBits
    = go xs (remainingBits - (x ^. bitWidth))

  go _xs remainingBits =
    Left
    $ "Continuity check failed with remaining bits: "
    <> show remainingBits
    <> " for device "
    <> d ^. name
    <> " for "
    <> p ^. name
    <> "."
    <> r ^. name

-- | Check all devices registers for continuity
checkDeviceRegisterContinuity
  :: Device
  -> Either String Device
checkDeviceRegisterContinuity d =
  let
    res =
      concatMap
        (\p ->
          map
            (continuityCheckReg d p)
            (p ^. registers)
        )
      (devicePeripherals d)
  in
    case res of
      _ | all Data.Either.isRight res -> pure d
      _ | otherwise -> Left $ unlines $ Data.Either.lefts res

mapPeriphs :: (Peripheral -> b) -> Device -> [b]
mapPeriphs f Device{..} = map f devicePeripherals

mapRegs :: (Register -> b) -> Peripheral -> [b]
mapRegs f Peripheral{..} = map f periphRegisters

mapFields :: (Field -> b) -> Register -> [b]
mapFields f Register{..} = map f regFields

mapDevFields :: (Field -> b) -> Device -> [b]
mapDevFields f d =
    concat
  $ concat
  $ flip mapPeriphs d
  $ mapRegs
  $ mapFields f

-- | Get peripheral by groupName
getPeriphByGroup :: String -> Device -> Peripheral
getPeriphByGroup name' dev =
  case filterLowerBy name' periphGroupName (devicePeripherals dev) of
    [] -> error $ "getPeriphByGroup, peripheral " ++ name' ++ " not found"
    [p] -> p
    ps -> case filter (Data.Maybe.isNothing . periphDerivedFrom) ps of
      [] -> error $ "getPeriphByGroup: No non-derived peripheral found for " ++ name'
      [p] -> p
      (p:_xs) -> p
       -- TODO: warn?
       -- error $ "getPeriphByGroup: Multiple non-derived peripheral found for " ++ name

-- | Get peripheral by name
getPeriph :: String -> Device -> Peripheral
getPeriph name' dev =
  Safe.headNote ("getPeriph " ++ name')
  . filterLowerBy name' periphName $ devicePeripherals dev

-- | Get peripheral by name iff found, Nothing otherwise
getPeriphMay :: String -> Device -> Maybe Peripheral
getPeriphMay name' dev =
  Safe.headMay
  . filterLowerBy name' periphName $ devicePeripherals dev

-- | Get register of the peripheral by their names iff found, Nothing otherwise
getPeriphRegMay :: String -> Peripheral -> Maybe Register
getPeriphRegMay rName =
  Safe.headMay
  . filterLowerBy rName regName . periphRegisters

-- | Filter elements matching lowercased `eqTo` after applying `by`
filterLowerBy :: String -> (a -> String) -> [a] -> [a]
filterLowerBy eqTo by =
  filter
  $ (== map Data.Char.toLower eqTo)
    . map Data.Char.toLower
    . by

-- | Get peripheral by name or its parent peripheral if it's
-- a derived peripheral (for example USART2 is typically derived from USART1)
getPeriphFollow :: String -> Device -> Either String Peripheral
getPeriphFollow pName dev = case getPeriphMay pName dev of
  Nothing -> Left $ "No peripheral found: " ++ pName
  Just p  -> case periphDerivedFrom p of
    Nothing -> Right p
    Just fromName -> case getPeriphMay fromName dev of
      Nothing -> Left $ "Parent peripheral not found: " ++ fromName ++ " for peripheral " ++ pName
      Just parentPeriph -> Right parentPeriph

-- | Get registers of the peripheral
getPeriphRegs :: String -> Device -> Either String [Register]
getPeriphRegs pName dev = periphRegisters <$> getPeriphFollow pName dev

-- | Get specific register of the peripheral
-- Follows derived from.
getPeriphReg :: String -> String -> Device -> Either String Register
getPeriphReg pName rName dev =
  either
    Left
    (maybeToEither errMsg . getPeriphRegMay rName)
    $ getPeriphFollow pName dev
  where
    errMsg = "No register found: " ++ rName ++ " for peripheral " ++ pName

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither msg m = case m of
  Just x -> Right x
  Nothing -> Left msg

-- | Get address of the specific register of the peripheral with `pName`
getPeriphRegAddr :: String -> String -> Device -> Either String Int
getPeriphRegAddr pName rName dev =
  (\p r -> periphBaseAddress p + regAddressOffset r)
  <$> maybeToEither errMsg (getPeriphMay pName dev)
  <*> getPeriphReg pName rName dev
  where
    errMsg = "No peripheral found " ++ pName

-- | Get fields of the specific register of the peripheral with `pName`
getPeriphRegFields
  :: String -- ^ Peripheral name
  -> String -- ^ Register name
  -> Device
  -> Either String [Field]
getPeriphRegFields pName rName dev =
  regFields <$> getPeriphReg pName rName dev

getReg
  :: String -- ^ Peripheral name
  -> String -- ^ Register name
  -> Device
  -> Register
getReg pName rName dev =
  Safe.headNote "getReg"
  . filter((==rName) . regName)
  . periphRegisters
  $ getPeriph pName dev

getRegFields
  :: String -- ^ Peripheral name
  -> String -- ^ Register name
  -> Device
  -> [Field]
getRegFields pName rName dev =
  regFields
  $ getReg pName rName dev

-- | Get value of specific @Field@ according to input `x`
getFieldVal :: (Bits a, Num a) => a -> Field -> a
getFieldVal x f = (x `shiftR` fieldBitOffset f) .&. (2 ^ fieldBitWidth f - 1)

-- | Decode integer `x` according to Fields `fs`
getFieldValues :: (Bits a, Num a) => a -> [Field] -> [(a, Field)]
getFieldValues x fs = zip (map (getFieldVal x) fs) fs

-- | Check if any reserved field has value other than 0
anyReservedSet :: (Eq a, Num a) => [(a, Field)] -> Bool
anyReservedSet = any (\(val, f) -> val /= 0 && fieldReserved f)

-- | Get memory map of the device according to its perhiperal addresses
getDevMemMap :: Device -> [(String, String)]
getDevMemMap Device{..} =
  map
    (liftM2 (,) (Data.Bits.Pretty.formatHex . periphBaseAddress) periphName)
    devicePeripherals

registerNames :: String -> Device -> [String]
registerNames pName dev =
  map
    regName . periphRegisters
    $ getPeriph pName dev

fieldNames :: String -> String -> Device -> [String]
fieldNames rName pName dev =
  map
    fieldName
    $ getRegFields pName rName dev

-- * Sorting

-- | Sort everything by memory address
sortDeviceByAddresses :: Device -> Device
sortDeviceByAddresses =
    over
      peripherals
      (Data.List.sortOn (view baseAddress))
  . over
      (peripherals . traverse . registers)
      (Data.List.sortOn (view addressOffset))
  . over
      (peripherals . traverse . registers . traverse . fields)
      (reverse . Data.List.sortOn (view bitOffset))

-- | Sort everything by name
sortDeviceByNames :: Device -> Device
sortDeviceByNames =
    over
      peripherals
      (Data.List.sortOn (view name))
  . over
      (peripherals . traverse . registers)
      (Data.List.sortOn (view name))
  . over
      (peripherals . traverse . registers . traverse . fields)
      (Data.List.sortOn (view name))

-- * Interrupts

fillMissingInterrupts
  :: [Interrupt]
  -> [Interrupt]
fillMissingInterrupts isrs =
  isrs
  ++ (map filler $ missingInterrupts)
  where
    filler x = Interrupt {
       interruptName = "Undefined" ++ show x
     , interruptValue = x
     , interruptDescription = "Undefined interrupt (padding only)"
     }
    missingInterrupts =
      let
        vals = map interruptValue isrs
      in
          Data.Set.toList
        $ Data.Set.difference
            (Data.Set.fromList [0 .. maximum vals])
            (Data.Set.fromList vals)
