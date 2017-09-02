module FormatSpec
  ( main
  , spec
  ) where

import Protolude
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Data.String.Here
import Helper

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

spec :: Spec
spec =
  it "works" $ do
    assertFormat
      [here|
module Main where

import Control.Monad.Eff.Console

main = log "Hello, World!"
|]
    assertFormat
      [here|
module Main where

import Control.Monad.Eff.Console

main = log "Hello, World!"
|]
