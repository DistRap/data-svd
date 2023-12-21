{-# LANGUAGE RecordWildCards #-}

module Data.SVD.IO
  ( parseSVD
  , parseSVDOptions
  , parseSVDPeripherals
  ) where

import Data.Default.Class (Default(def))
import Data.SVD.Types (Device, Peripheral)
import Text.XML.HXT.Core (readDocument, runX, (>>>))

import qualified Data.Bool
import qualified Data.SVD.Dim
import qualified Data.SVD.Parse
import qualified Data.SVD.Util

data SVDSort
  = SVDSort_DontSort
  | SVDSort_SortByNames
  | SVDSort_SortByAddresses
  deriving (Show)

data SVDOptions = SVDOptions
  { svdOptionsExpand :: Bool
  -- ^ Expand dimensions and clusters
  , svdOptionsSort :: SVDSort
  -- ^ Sorting
  } deriving (Show)

instance Default SVDOptions where
  def = SVDOptions
    { svdOptionsExpand = True
    , svdOptionsSort = SVDSort_SortByAddresses
    }

parseSVDOptions
  :: SVDOptions
  -> String
  -> IO (Either String Device)
parseSVDOptions SVDOptions{..} f = do
  res <- runX (readDocument [] f >>> Data.SVD.Parse.svd)
  case res of
    [] -> pure $ Left "No device parsed"
    [x] ->
      pure
        . pure
        . case svdOptionsSort of
            SVDSort_DontSort -> id
            SVDSort_SortByAddresses -> Data.SVD.Util.sortDeviceByAddresses
            SVDSort_SortByNames -> Data.SVD.Util.sortDeviceByNames
        . Data.Bool.bool
            id
            Data.SVD.Dim.expandDevice
            svdOptionsExpand
        $ x
    _ -> pure $ Left "Multiple devices parsed"

parseSVD
  :: String
  -> IO (Either String Device)
parseSVD = parseSVDOptions def

parseSVDPeripherals :: String -> IO (Either String [Peripheral])
parseSVDPeripherals f = do
  res <- runX (readDocument [] f >>> Data.SVD.Parse.svdPeripherals)
  case res of
    [] -> pure $ Left "No peripherals parsed"
    [x] -> pure $ Right x
    _ -> pure $ Left "Multiple devices parsed"
