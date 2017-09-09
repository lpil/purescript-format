module Helper where

import qualified Language.PureScript.Format as Format
import Protolude
import Test.Hspec (Expectation, shouldBe)

assertFormat :: Text -> Expectation
assertFormat source =
  let formatted = Format.format 40 source
  in formatted `shouldBe` Right source
