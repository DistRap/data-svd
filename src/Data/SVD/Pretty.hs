{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SVD.Pretty
  (
  -- * Helpers
    ppList
  , displayPretty
  , displayCompact
  -- * Shorthand
  , displayDevice
  , displayDeviceInfo
  , displayPeripheral
  , displayRegister
  , displayMemMap
  , displayMemMapCompact
  , displayDevISR
  , displayISRs
  -- * Pretty printers
  , ppDevice
  , ppPeriph
  , ppReg
  , ppHex
  -- ** Interrupts
  , ppDevISR
  , ppISR
  -- ** Terse output
  , ppDeviceInfo
  , ppPeriphName
  , shortField
  -- ** MemMap
  , ppMem
  )
  where

import Data.Char (toLower)
import Data.SVD.Types
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Terminal (AnsiStyle, Color(..), color)

import qualified Data.Bits.Pretty
import qualified Data.Text
import qualified Prettyprinter.Render.Terminal

-- * Helpers

ppList :: (a -> Doc AnsiStyle) -> [a] -> Doc AnsiStyle
ppList pp x = vcat $ map pp x

displayPretty :: Doc AnsiStyle -> String
displayPretty =
    Data.Text.unpack
  . Prettyprinter.Render.Terminal.renderStrict
  . layoutPretty defaultLayoutOptions

displayCompact :: Doc AnsiStyle -> String
displayCompact =
    renderString
  . layoutCompact

-- * Shorthand

displayDevice :: Device -> String
displayDevice = displayPretty . ppDevice

displayDeviceInfo :: Device -> String
displayDeviceInfo = displayPretty . ppDeviceInfo

displayPeripheral :: Peripheral -> String
displayPeripheral = displayPretty . ppPeriph

displayRegister :: Register -> String
displayRegister = displayPretty . ppReg

displayMemMap :: [(String, String)] -> String
displayMemMap = displayPretty . ppList ppMem

displayMemMapCompact :: [(String, String)] -> String
displayMemMapCompact = displayCompact . ppList ppMem

displayDevISR :: Device -> String
displayDevISR = displayPretty . ppDevISR

displayISRs :: [Interrupt] -> String
displayISRs = displayPretty . ppList ppISR

-- * Pretty printers

ppDevice :: Device -> Doc AnsiStyle
ppDevice Device{..} =
  (annotate (color Red) $ pretty deviceName)
  <> line
  <> indent 2 (ppList ppPeriph devicePeripherals)

ppPeriph :: Peripheral -> Doc AnsiStyle
ppPeriph Peripheral{..} =
      hardline
  <>  annotate (color Yellow)
        (pretty periphName)
  <+> annotate (color White)
        (ppHex periphBaseAddress)
  <+> annotate (color Magenta)
        (pretty periphDescription)
  <>  line
  <>  indent 2 (ppList ppReg periphRegisters)
  <>  maybe
        mempty
        (\x ->
          indent 2
           $   line
           <>  pretty ("Derived from" :: String)
           <+> pretty x
        )
        periphDerivedFrom

ppReg :: Register -> Doc AnsiStyle
ppReg Register{..} =
  hardline
  <>  annotate (color Blue)
        (pretty regName)
  <+> annotate (color White)
        (ppHex regAddressOffset)
  <+> annotate (color Cyan)
        (pretty '-' <+> (pretty regDescription))
  <>  line
  <>  indent 2
        (ppList ppField regFields)

ppHex :: Int -> Doc AnsiStyle
ppHex = pretty . Data.Bits.Pretty.formatHex

rpad :: Int -> String -> String
rpad m xs = take m $ xs ++ repeat ' '

ppField :: Field -> Doc AnsiStyle
ppField Field{..} =
      annotate (color Green)
        (pretty $ rpad 25 fieldName)
  <+> pretty ("::" :: String)
  <+> ppWidthPad 7 fieldBitWidth
  <+> annotate
        (color Cyan)
        (pretty $ " -- " ++ fieldDescription)

ppWidthPad
  :: Int
  -> Int
  -> Doc AnsiStyle
ppWidthPad m 1 = pretty $ rpad m "Bit"
ppWidthPad m x = pretty $ rpad m $ "Bits " ++ show x

-- ** Interrupts

ppDevISR :: Device -> Doc AnsiStyle
ppDevISR Device{..} = ppList ppPeriphISR devicePeripherals

ppPeriphISR :: Peripheral -> Doc AnsiStyle
ppPeriphISR Peripheral{..} =
  indent 2 (ppList ppISR periphInterrupts)
--  <//> (maybe empty (\x -> string "Derived from" <+> string x) periphDerivedFrom)

ppISR :: Interrupt -> Doc AnsiStyle
ppISR Interrupt{..} =
  indent 2
    (
          "|"
      <+> pretty interruptName
      <>  " -- " <> pretty interruptValue <+> pretty interruptDescription
    )

-- ** Terse output

ppDeviceInfo :: Device -> Doc AnsiStyle
ppDeviceInfo Device{..} =
     annotate (color Red)
       (pretty deviceName)
  <> line
  <> indent 2
       (ppList ppPeriphName devicePeripherals)

ppPeriphName :: Peripheral -> Doc AnsiStyle
ppPeriphName Peripheral{..} =
  annotate (color Yellow)
    (pretty periphName)

shortField :: Field -> String
shortField Field{..} = unwords [
  fieldName
  , "offset"
  , show fieldBitOffset
  , "width"
  , show fieldBitWidth ]

-- ** MemMap

ppMem :: (String, String) -> Doc AnsiStyle
ppMem (addr, periph) =
     name <> " :: Integer"
  <> line
  <> name
  <> " = "
  <> pretty addr
  where
    name = pretty (map toLower periph) <> "_periph_base"


