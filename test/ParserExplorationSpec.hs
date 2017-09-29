module ParserExplorationSpec
  ( main
  , spec
  ) where

import Protolude hiding (group, list)
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Control.Arrow ((>>>))
import Data.String.Here
import Text.PrettyPrint.Leijen.Text hiding (list)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

render :: Int -> Doc -> Text
render maxWidth = renderPretty 0.9 maxWidth >>> displayTStrict

spec :: Spec
spec = do
  describe "pretty printing with Wadler's algebra" $ do
    it "line with long width" $ do
      let doc = mconcat ["Hello", line, "world!"]
      render 100 doc `shouldBe`
        [here|
Hello
world!
|]
    it "line with short width" $ do
      let doc = mconcat ["Hello", line, "world!"]
      render 5 doc `shouldBe`
        [here|
Hello
world!
|]
    it "grouped line with long width" $ do
      let doc = group $ mconcat ["Hello", line, "world!"]
      render 100 doc `shouldBe`
        [here|
Hello world!
|]
    it "grouped line with short width" $ do
      let doc = group $ mconcat ["Hello", line, "world!"]
      render 5 doc `shouldBe`
        [here|
Hello
world!
|]
    it "lines with medium width" $ do
      let doc = mconcat ["a", line, "b", line, "c", line, "d"]
      render 5 doc `shouldBe`
        [here|
a
b
c
d
|]
    it "lines with medium width, some grouped" $ do
      let doc = mconcat ["a", line, group $ mconcat ["b", line, "c", line, "d"]]
      render 6 doc `shouldBe`
        [here|
a
b c d
|]
    it "list" $ do
      let elems = mconcat ["1", line, "2", line, "3", line, "4"]
      let list = group $ mconcat ["[", nest 1 elems, "]"]
      render 10 list `shouldBe`
        [here|
[1 2 3 4]
|]
      render 9 list `shouldBe`
        [here|
[1
 2
 3
 4]
|]
    it "list assigned to name" $ do
      let elems = mconcat ["1", line, "2", line, "3", line, "4"]
      let list = group $ mconcat ["[", nest 1 elems, "]"]
      let var = group $ mconcat ["let x =", nest 2 $ line <> list]
      render 19 var `shouldBe`
        [here|
let x = [1 2 3 4]
|]
      render 12 var `shouldBe`
        [here|
let x =
  [1 2 3 4]
|]
      render 10 var `shouldBe`
        [here|
let x =
  [1
   2
   3
   4]
|]
    it "list assigned to name, using helper functions" $ do
      let elems = vsep ["1", "2", "3", "4"]
      let list = group $ brackets $ nest 1 elems
      let body = nest 2 (line <> list)
      let var = group ("let x =" <> body)
      render 19 var `shouldBe`
        [here|
let x = [1 2 3 4]
|]
      render 12 var `shouldBe`
        [here|
let x =
  [1 2 3 4]
|]
      render 10 var `shouldBe`
        [here|
let x =
  [1
   2
   3
   4]
|]
    it "list, preceeding comma" $ do
      let elems =
            mconcat
              [ "1"
              , linebreak
              , ", "
              , "2"
              , linebreak
              , ", "
              , "3"
              , linebreak
              , ", "
              , "4"
              , linebreak
              ]
      let list = group (brackets elems)
      let body = nest 2 (line <> list)
      let var = group ("let x =" <> body)
      render 22 var `shouldBe`
        [here|
let x = [1, 2, 3, 4]
|]
      render 15 var `shouldBe`
        [here|
let x =
  [1, 2, 3, 4]
|]
-- This is wrong. Missing space after the [
      render 6 var `shouldBe`
        [here|
let x =
  [1
  , 2
  , 3
  , 4
  ]
|]
