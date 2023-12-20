{-# LANGUAGE DeriveGeneric #-}

module Data.SVD.Types
  ( AccessType(..)
  , toAccessType
  , showAccessType
  , AddressBlock(..)
  , Cluster(..)
  , Device(..)
  , Dimension(..)
  , DimensionIndex(..)
  , Interrupt(..)
  , Peripheral(..)
  , Register(..)
  , Field(..)
  ) where

import Data.Default.Class (Default(def))
import Data.Serialize (Serialize)
import GHC.Generics (Generic)

data Device = Device {
    deviceName            :: String
  , deviceVersion         :: String
  , deviceDescription     :: String
  , deviceAddressUnitBits :: Int
  , deviceWidth           :: Int
  , deviceSize            :: Int
  , deviceResetValue      :: Int
  , deviceResetMask       :: Int
  , devicePeripherals     :: [Peripheral]
  } deriving (Generic, Eq, Ord, Show)

instance Default Device where
  def = Device
    { deviceName            = "defaultDev"
    , deviceVersion         = mempty
    , deviceDescription     = mempty
    , deviceAddressUnitBits = 0
    , deviceWidth           = 0
    , deviceSize            = 0
    , deviceResetValue      = 0
    , deviceResetMask       = 0
    , devicePeripherals     = []
    }

instance Serialize Device

data Peripheral = Peripheral {
    periphName         :: String
  , periphDescription  :: String
  , periphDerivedFrom  :: Maybe String
  , periphGroupName    :: String
  , periphBaseAddress  :: Int
  , periphAddressBlock :: Maybe AddressBlock
  , periphInterrupts   :: [Interrupt]
  , periphRegisters    :: [Register]
  , periphClusters     :: [Cluster]
  } deriving (Generic, Eq, Ord, Show)

instance Default Peripheral where
  def = Peripheral
    { periphName         = "defaultPeriph"
    , periphDescription  = mempty
    , periphDerivedFrom  = Nothing
    , periphGroupName    = mempty
    , periphBaseAddress  = 0
    , periphAddressBlock = Nothing
    , periphInterrupts   = []
    , periphRegisters    = []
    , periphClusters     = []
    }

instance Serialize Peripheral

data AddressBlock = AddressBlock {
    addressBlockOffset :: Int
  , addressBlockSize   :: Int
  , addressBlockUsage  :: String
  } deriving (Generic, Eq, Ord, Show)

instance Serialize AddressBlock

data Interrupt = Interrupt {
    interruptName        :: String
  , interruptDescription :: String
  , interruptValue       :: Int
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Interrupt

data DimensionIndex
  = DimensionIndex_FromTo Int Int
  | DimensionIndex_List [String]
  deriving (Generic, Eq, Ord, Show)

instance Serialize DimensionIndex

data Dimension = Dimension {
    dimensionSize      :: Int
  , dimensionIncrement :: Int
  , dimensionIndex     :: DimensionIndex
  } deriving (Generic, Eq, Ord, Show)

instance Serialize Dimension

-- | Cluster describes a sequence of neighboring registers within a peripheral.
-- A <cluster> specifies the addressOffset relative to the baseAddress of the grouping element.
-- All <register> elements within a <cluster> specify their addressOffset relative to the cluster base address
-- (<peripheral.baseAddress> + <cluster.addressOffset>).
data Cluster = Cluster {
    clusterName          :: String
  , clusterDimension     :: Maybe Dimension
  , clusterDescription   :: String
  , clusterAddressOffset :: Int
  , clusterRegisters     :: [Register]
  -- unused, expansion not yet implemented
  -- but also not quite present in any SVD we've seen
  , clusterNested        :: [Cluster]
  } deriving (Generic, Eq, Ord, Show)

instance Default Cluster where
  def = Cluster
    { clusterName          = "defaultCluster"
    , clusterDescription   = mempty
    , clusterDimension     = Nothing
    , clusterAddressOffset = 0
    , clusterRegisters     = []
    , clusterNested        = []
    }

instance Serialize Cluster

data Register = Register {
    regName          :: String
  , regDisplayName   :: String
  , regDimension     :: Maybe Dimension
  , regDescription   :: String
  , regAddressOffset :: Int
  , regSize          :: Int
  , regAccess        :: AccessType
  , regResetValue    :: Maybe Int
  , regFields        :: [Field]
  } deriving (Generic, Eq, Ord, Show)

instance Default Register where
  def = Register
    { regName          = "defaultRegister"
    , regDisplayName   = mempty
    , regDimension     = Nothing
    , regDescription   = mempty
    , regAddressOffset = 0
    , regSize          = 0
    , regAccess        = ReadOnly
    , regResetValue    = Nothing
    , regFields        = []
    }

instance Serialize Register

data AccessType
  = ReadOnly
  | WriteOnly
  | ReadWrite
  | WriteOnce
  | ReadWriteOnce
  deriving (Generic, Eq, Ord, Show)

instance Serialize AccessType

data Field = Field {
    fieldName        :: String
  , fieldDescription :: String
  , fieldDimension   :: Maybe Dimension
  , fieldBitOffset   :: Int
  , fieldBitWidth    :: Int
  , fieldReserved    :: Bool  -- so we can add reserved fields to the list
  , fieldRegType     :: Maybe String  -- ivory register type
  } deriving (Generic, Eq, Ord, Show)

instance Default Field where
  def = Field
    { fieldName        = "defaultField"
    , fieldDescription = mempty
    , fieldDimension   = Nothing
    , fieldBitOffset   = 0
    , fieldBitWidth    = 0
    , fieldReserved    = False
    , fieldRegType     = Nothing
    }

instance Serialize Field

toAccessType :: String -> AccessType
toAccessType "read-only"      = ReadOnly
toAccessType "write-only"     = WriteOnly
toAccessType "read-write"     = ReadWrite
toAccessType "writeOnce"      = WriteOnce
toAccessType "read-writeOnce" = ReadWriteOnce
toAccessType x                = error $ "Unable to read AccessType" ++ x

showAccessType :: AccessType -> String
showAccessType ReadOnly       = "read-only"
showAccessType WriteOnly      = "write-only"
showAccessType ReadWrite      = "read-write"
showAccessType WriteOnce      = "writeOnce"
showAccessType ReadWriteOnce  = "read-writeOnce"
