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
  describe "Language.PureScript.Format.format" $ do
    it "formats empty modules" $ do
      assertFormat "module Main where"
      assertFormat "module Data.Text where"
      assertFormat "module I.Give.Modules.Really.Really.Long.Names where"
    it "formats simple unqualified imports" $ do
      assertFormat
        [here|
module Main where

import Data.Text
import Data.Char
|]
    it "formats imports that specify functions" $ do
      assertFormat
        [here|
module Main where

import Data.Char (ok, ko)
|]
      assertFormat
        [here|
module Main where

import Control.Monad.Eff.Console
       (log, vlog)
|]
      -- assertFormat
      --   [here|
-- module Main where
-- import Control
      --  (one, two, three, four)
-- |]
      -- assertFormat
      --   [here|
-- module Main where
-- import Numbers
      --  (one, two, three, four, five,
      --   six, seven, eight, nine, ten,
      --   eleven, twelve)
-- |]
      -- assertFormat
      --   [here|
-- module M where
-- import Magic (Spell, (+++),
      --         Familiar(Cat, Owl, Toad))
-- |]
