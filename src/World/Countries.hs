{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module World.Countries where

import Data.Hashable
import Data.Data
import GHC.Generics (Generic)
import World.Countries.TH

$(createDeclarations)