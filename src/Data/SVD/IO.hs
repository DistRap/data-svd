{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SVD.IO
  ( parseSVD
  , parseSVDOptions
  , SVDOptions(..)
  ) where

import Data.Default.Class (Default(def))
import Data.Hashable (Hashable)
import Data.SVD.Types (Device)
import GHC.Generics (Generic)
import Text.XML.HXT.Core (readString, runX, (>>>))

import qualified Control.Monad
import qualified Data.Bool
import qualified Data.ByteString.Char8
import qualified Data.Either
import qualified Data.Hashable
import qualified Data.Serialize
import qualified Data.SVD.Dim
import qualified Data.SVD.Parse
import qualified Data.SVD.Util
import qualified System.Directory

data SVDSort
  = SVDSort_DontSort
  | SVDSort_SortByNames
  | SVDSort_SortByAddresses
  deriving (Eq, Ord, Generic, Show)

instance Hashable SVDSort

data SVDOptions = SVDOptions
  { svdOptionsAddReservedFields :: Bool
  -- ^ Fill in dummy reserved fields where
  -- holes would be in registers
  , svdOptionsCache :: Bool
  -- ^ Cache parsed SVD in /tmp
  -- based on a hash of the input svd file
  , svdOptionsCheckContinuity :: Bool
  -- ^ Check register continuity
  , svdOptionsExpand :: Bool
  -- ^ Expand dimensions and clusters
  , svdOptionsSort :: SVDSort
  -- ^ Sorting
  } deriving (Eq, Ord, Generic, Show)

instance Default SVDOptions where
  def = SVDOptions
    { svdOptionsAddReservedFields = True
    , svdOptionsCache = True
    , svdOptionsCheckContinuity = True
    , svdOptionsExpand = True
    , svdOptionsSort = SVDSort_SortByAddresses
    }

instance Hashable SVDOptions

parseSVDOptions
  :: SVDOptions
  -> String
  -> IO (Either String Device)
parseSVDOptions opts@SVDOptions{..} f = do
  s <- readFile f
  -- If caching is enabled we hash the input
  -- string + options and try to load
  -- serialized binary from cache if it exists
  -- or create one if not for further invocations
  let fHash = Data.Hashable.hash s
      optsHash = Data.Hashable.hash opts
      caFile =
        "/tmp/svdCache-"
        <> show fHash
        <> "-"
        <> show optsHash

  if not svdOptionsCache
  then parseSVDFromString opts s
  else do
    hasCached <- System.Directory.doesFileExist caFile
    if hasCached
    then
      Data.Serialize.decode
      <$> Data.ByteString.Char8.readFile caFile
      >>= \case
        Left e ->
          error
            $ "Can't decode cached svd from "
            <> caFile
            <> " error was "
            <> e
        Right x -> pure x
    else do
      res <- parseSVDFromString opts s
      Control.Monad.unless
        (Data.Either.isLeft res)
        $ Data.ByteString.Char8.writeFile
            caFile
            $ Data.Serialize.encode res
      pure res

parseSVDFromString
  :: SVDOptions
  -> String
  -> IO (Either String Device)
parseSVDFromString SVDOptions{..} s = do
  res <- runX (readString [] s >>> Data.SVD.Parse.svd)
  case res of
    [] -> pure $ Left "No device parsed"
    [x] ->
          pure
        . Data.Bool.bool
            Right
            Data.SVD.Util.checkDeviceRegisterContinuity
            svdOptionsCheckContinuity
        . case svdOptionsSort of
            SVDSort_DontSort -> id
            SVDSort_SortByAddresses -> Data.SVD.Util.sortDeviceByAddresses
            SVDSort_SortByNames -> Data.SVD.Util.sortDeviceByNames
        . Data.Bool.bool
            id
            Data.SVD.Util.addReservedFields
            svdOptionsAddReservedFields
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
