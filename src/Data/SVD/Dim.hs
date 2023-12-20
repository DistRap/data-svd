{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SVD.Dim
  ( expandDim
  , expandCluster
  , expandField
  , expandRegister
  ) where

import Control.Lens ((^.), set)
import Data.SVD.Lens
import Data.SVD.Types

-- * Dimension expansion

-- Expand @Cluster@, @Field@, @Register@ into multiples
-- according to its @Dimension@
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
    Nothing -> mempty
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

expandField :: Field -> [Field]
expandField = expandDim (^. bitOffset) (set bitOffset)

expandCluster :: Cluster -> [Cluster]
expandCluster = expandDim (^. addressOffset) (set addressOffset)

expandRegister :: Register -> [Register]
expandRegister = expandDim (^. addressOffset) (set addressOffset)
