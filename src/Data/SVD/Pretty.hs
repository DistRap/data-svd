{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.SVD.Pretty where

import Data.Char (toLower)
import Data.SVD.Types
import Data.Word
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import qualified Data.Bits.Pretty

ppList :: (a -> Doc) -> [a] -> Doc
ppList pp x = vcat $ map pp x

ppDevice :: Device -> String
ppDevice res = displayS (renderPretty 0.4 80 (ppDevice' res)) ""

ppDeviceInfo :: Device -> String
ppDeviceInfo res = displayS (renderPretty 0.4 80 (ppDeviceInfo' res)) ""

ppPeripheral :: Peripheral -> String
ppPeripheral res = displayS (renderPretty 0.4 80 (ppPeriph res)) ""

ppRegister :: Register -> String
ppRegister res = displayS (renderPretty 0.4 80 (ppReg res)) ""

ppDevice' :: Device -> Doc
ppDevice' Device{..} =
  (red $ string deviceName)
  <$$> indent 2 (ppList ppPeriph devicePeripherals)

ppPeriph :: Peripheral -> Doc
ppPeriph Peripheral{..} =
  hardline
  <> (yellow $ string periphName)
  <+> (white $ ppHex periphBaseAddress)
  <+> (magenta $ string periphDescription)
  <$$> indent 2 (ppList ppReg periphRegisters)
  <//> (maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom)

ppReg :: Register -> Doc
ppReg Register{..} =
  hardline
  <> (blue $ string regName)
  <+> (white $ ppHex regAddressOffset)
  <+> (cyan $ char '-' <+> (string regDescription))
  <$$> indent 2 (ppList ppField regFields)

ppIvoryReg
  :: Peripheral
  -> Register
  -> Doc
ppIvoryReg Peripheral{..} Register{..} =
  hardline
  <> (red $ string $ "-- " ++ periphName ++ periphDescription)
  <> hardline
  <> (red $ string "[ivory|\n")
  <+> (string "bitdata ")
  <> (blue $ string $ upcaseRegName)
  <+> (string $ ":: Bits " ++ (show regSize) ++ " = ")
  <> (blue $ string lowcaseRegName)
  <$$> indent 2 ((vcat $ zipWith (<+>) pattern (ppField <$> fmap (prefixRegField $ lowcaseRegName ++ "_") regFields))
                  <$$> (string "}"))
  <$$> (red $ string "|]")
  <$$> mkPeriph
    where
      pattern = (string "{"):(repeat $ string ",")
      prefixRegField :: String -> Field -> Field
      prefixRegField prefix f = f {fieldName = (prefix ++fieldName f)}
      upcaseRegName = mconcat [periphName, "_", regName]
      lowcaseRegName = toLower <$> upcaseRegName
      mkPeriph =
        hardline
        <> (string $ "reg" ++ upcaseRegName ++ " :: BitDataReg " ++ upcaseRegName)
        <> hardline
        <> (string $ mconcat ["reg",
                            upcaseRegName,
                            " = mkBitDataRegNamed (",
                            (toLower <$> periphName),
                            "_periph_base + ", Data.Bits.Pretty.formatHex regAddressOffset, ")",
                            " \"", lowcaseRegName, "\""
                            ])

ppHex :: Int -> Doc
ppHex = text . Data.Bits.Pretty.formatHex

rpad :: Int -> String -> String
rpad m xs = take m $ xs ++ repeat ' '

ppField :: Field -> Doc
ppField Field{..} =
  (green $ string $ rpad 25 (toLower <$> fieldName))
  <+> string "::"
  <+> ppWidthPad 7 fieldBitWidth
  <+> cyan (string $ " -- " ++ fieldDescription)

ppWidth :: Int -> Doc
ppWidth 1 = string "Bit"
ppWidth x = string "Bits" <+> int x

ppWidthPad
  :: Int
  -> Int
  -> Doc
ppWidthPad m 1 = string $ rpad m "Bit"
ppWidthPad m x = string $ rpad m $ "Bits " ++ show x

-- * ISR

ppDevISR :: Device -> String
ppDevISR res = displayS (renderPretty 0.4 80 (ppDevISR' res)) ""

ppDevISR' :: Device -> Doc
ppDevISR' Device{..} = (ppList ppPeriphISR devicePeripherals)

ppPeriphISR :: Peripheral -> Doc
ppPeriphISR Peripheral{..} =
  indent 2 (ppList ppISR periphInterrupts)
--  <//> (maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom)

ppISRs :: [Interrupt] -> String
ppISRs res = displayS (renderPretty 0.4 80 (ppList ppISR res)) ""

ppISR :: Interrupt -> Doc
ppISR Interrupt{..} = indent 2 (
  "|"
  <+> string interruptName
  <> " -- " <> int interruptValue <+> string interruptDescription
  )

-- Terse output

ppDeviceInfo' :: Device -> Doc
ppDeviceInfo' Device{..} =
  (red $ string deviceName)
  <$$> indent 2 (ppList ppPeriphName devicePeripherals)

ppPeriphName :: Peripheral -> Doc
ppPeriphName Peripheral{..} = (yellow $ string periphName)

-- * MemMap

ppMemMap :: [(String, String)] -> String
ppMemMap res = displayS (renderPretty 0.4 80 (ppList ppMem res)) ""

ppMem :: (String, String) -> Doc
ppMem (addr, periph) =
     name <> " :: Integer"
  </> name
  <> " = "
  <> string addr
  where name = string (map toLower periph) <> "_periph_base"

shortField :: Field -> String
shortField Field{..} = unwords [
  fieldName
  , "offset"
  , show fieldBitOffset
  , "width"
  , show fieldBitWidth ]

-- | Print currently set (non-zero) fields
printSetFields :: (Show a, Eq a, Num a) => [(a, Field)] -> String
printSetFields = unlines . map printSetField . filterSet

printSetField :: (Show a, Eq a, Num a) => (a, Field) -> String
printSetField (_, f) | fieldBitWidth f == 1 = concat ["Bit ", show (fieldBitOffset f), " ", fieldName f]
printSetField (v, f) | otherwise = concat [
    "Bits ["
  , show (fieldBitOffset f)
  , ":"
  , show (fieldBitOffset f + fieldBitWidth f - 1)
  , "]"
  , " "
  , fieldName f
  , " value ", show v]

-- | Show `Field` with its range, e.g BRR[15:0] (16 bit wide)
showField :: Field -> String
showField f@Field{..} | fieldReserved = "◦" ++ (fieldRange f)
showField f@Field{..} | otherwise = fieldName ++ (fieldRange f)

-- | Datasheeeet like
fieldRange :: Field -> String
fieldRange Field{..} | fieldBitWidth == 1 = ""
fieldRange Field{..} | otherwise = concat ["[", show $ fieldBitWidth - 1, ":0]"]

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
