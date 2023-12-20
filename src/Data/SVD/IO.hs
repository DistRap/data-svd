module Data.SVD.IO
  ( parseSVD
  , parseSVDPeripherals
  ) where

import Data.SVD.Types (Device, Peripheral)
import Text.XML.HXT.Core (readDocument, runX, (>>>))

import qualified Data.SVD.Parse

parseSVD :: String -> IO (Either String Device)
parseSVD f = do
  res <- runX (readDocument [] f >>> Data.SVD.Parse.svd)
  case res of
    [] -> return $ Left "No device parsed"
    [x] -> return $ Right x
    _ -> return $ Left "Multiple devices parsed"

parseSVDPeripherals :: String -> IO (Either String [Peripheral])
parseSVDPeripherals f = do
  res <- runX (readDocument [] f >>> Data.SVD.Parse.svdPeripherals)
  case res of
    [] -> return $ Left "No peripherals parsed"
    [x] -> return $ Right x
    _ -> return $ Left "Multiple devices parsed"
