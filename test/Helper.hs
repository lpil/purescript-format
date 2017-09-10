module Helper where

import Language.PureScript.Format
import Protolude
import Test.Hspec (Expectation, shouldBe)

assertFormat :: Text -> Expectation
assertFormat source =
  let options = defaultOptions {maxWidth = 40}
      formatted = formatWith options source
  in formatted `shouldBe` Right source
