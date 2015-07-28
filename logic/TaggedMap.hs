{-# LANGUAGE StandaloneDeriving, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module TaggedMap where

import Data.Aeson.Types
import qualified Data.Map as M
import Data.Tagged
import Data.String (IsString)

instance FromJSON (M.Map k v) => FromJSON (M.Map (Tagged t k) v) where
    parseJSON = fmap (M.mapKeysMonotonic Tagged) . parseJSON


instance ToJSON (M.Map k v) => ToJSON (M.Map (Tagged t k) v) where
    toJSON = toJSON . M.mapKeysMonotonic  untag

deriving instance FromJSON v => FromJSON (Tagged k v)
deriving instance ToJSON v => ToJSON (Tagged k v)

-- Slight hack, which allows us to use "foo" instead of (Tagged "foo"), e.g.
-- when writing test input.
deriving instance IsString v => IsString (Tagged k v)

