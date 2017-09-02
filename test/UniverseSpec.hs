module UniverseSpec
  ( main
  , spec
  ) where

import Protolude
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  describe "the universe" $
  it "behaves as we expect" $ do
    1 `shouldBe` 1
    1 + 2 `shouldBe` 3
