{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SVD.Dim
  ( expandDevice
  -- * For testing
  , expandCluster
  , expandField
  , expandRegister
  ) where

import Control.Lens ((^.), set, over)
import Data.SVD.Lens
import Data.SVD.Types

-- * Dimension expansion

-- Expand @Cluster@, @Field@, @Register@ into multiples
-- according to its @Dimension@
--
-- If @Dimension@ is nothing return singleton with the original
expandDim
  :: ( HasName a String
     , HasDescription a String
     , HasDimension a (Maybe Dimension)
     )
  => (a -> Int) -- ^ Address offset or bit offset getter
  -> (Int -> a -> a) -- ^ Address offset or bit offset setter
  -> a
  -> [a]
expandDim getOffset setOffset element =
  case element ^. dimension of
    Nothing -> pure element
    Just dim ->
      let ixs = case dim ^. index of
            DimensionIndex_FromTo f t -> map show [f .. t]
            DimensionIndex_List l -> l

          gen z i ix =
            let nameTemplate = z ^. name
                descTemplate = z ^. description
                baseOffset = getOffset z
                template ('%':'s':xs) replacement = replacement ++ xs
                template (x:xs) replacement = x:(template xs replacement)
                template [] _ = mempty
            in
                setOffset
                  (baseOffset + dim ^. increment * i)
              . set
                  name
                  (template nameTemplate ix)
              . set
                  description
                  (template descTemplate ix)
              $ set
                  dimension
                  Nothing
                  z
      in
        [ gen element i ix | (i, ix) <- zip [0..] ixs ]

-- | Expand @Field@ into multiple fields if it has a @Dimension@
-- return just the field if not
expandField :: Field -> [Field]
expandField = expandDim (^. bitOffset) (set bitOffset)

-- | Expand @Cluster@ into multiple cluster if it has a @Dimension@
-- return just the cluster if not
expandCluster :: Cluster -> [Cluster]
expandCluster = expandDim (^. addressOffset) (set addressOffset)

-- | Expand @Register@ into multiple registers if it has a @Dimension@
-- return just the register if not
expandRegister :: Register -> [Register]
expandRegister = expandDim (^. addressOffset) (set addressOffset)

-- | Expand all fields of a register
expandRegFields :: Register -> Register
expandRegFields r =
  set
    fields
    (concatMap expandField (r ^. fields))
    r

-- | Expand all registers of a peripheral
expandPeriphRegisters :: Peripheral -> Peripheral
expandPeriphRegisters p =
  set
    registers
    (concatMap expandRegister (p ^. registers))
    p

-- | Expand all cluster of a peripheral
-- then eliminate all of them into registers
expandPeriphClusters :: Peripheral -> Peripheral
expandPeriphClusters p =
  set
    clusters
    mempty
  $ set
      registers
      (let
           expClusters =
             concatMap
               expandCluster
               (p ^. clusters)
        in
          (p ^. registers)
          ++ concatMap
              eliminateCluster
              expClusters
      )
      p

-- | Turn expanded @Cluster@ into @Register@s
-- adding its addressOffset to each registers addressOffset
eliminateCluster :: Cluster -> [Register]
eliminateCluster c =
  map
    (\r ->
      over
        addressOffset
        (+(c ^. addressOffset))
        r
    )
  $ c ^. registers

-- | Expand all dimensions and clusters
--
-- In order
-- - Expand and eliminate each cluster
-- - Expand fields of each register
-- - Expand each register
expandDevice :: Device -> Device
expandDevice =
    over
      (peripherals . traverse)
      expandPeriphRegisters
  . over
      (peripherals . traverse . registers . traverse)
      expandRegFields
  . over
      (peripherals . traverse)
      expandPeriphClusters
