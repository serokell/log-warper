-- | Tests for rolling logger

module Test.Wlog.RollingSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "Dummy" $ do
    prop "Dummy" True
