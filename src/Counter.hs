{-# LANGUAGE DeriveGeneric #-}

module Counter where

import Data.Aeson.Types
import GHC.Generics

newtype Counter = Counter {
  count :: Int
} deriving (Show, Eq, Generic)

instance ToJSON Counter
