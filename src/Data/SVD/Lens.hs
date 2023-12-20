{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.SVD.Lens where

import Control.Lens
import Data.SVD.Types

makeFields ''Device
makeLensesWith abbreviatedFields ''Peripheral
makeLensesWith abbreviatedFields ''Register
makeFields ''Field
makeFields ''Cluster
makeFields ''Dimension
makeFields ''Interrupt
makeFields ''AddressBlock
