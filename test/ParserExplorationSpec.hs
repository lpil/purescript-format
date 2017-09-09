module ParserExplorationSpec
  ( main
  , spec
  ) where

import Protolude
import Test.Hspec
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Control.Arrow ((>>>))
import Data.String.Here
import qualified Language.PureScript as PS
import qualified Text.Parsec as P (ParseError)

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} spec

tokenize :: Text -> Either P.ParseError [Text]
tokenize = PS.lex "file.purs" >>> fmap (map show)

parse :: Text -> Either P.ParseError PS.Module
parse source =
  PS.lex "file.purs" source >>= PS.runTokenParser "file.purs" PS.parseModule

spec :: Spec
spec = do
  let source =
        [here|
module Main where

import Control.Monad.Eff.Console

main = log "Hello, World!"
|]
  describe "purescript parser exploration" $ do
    it "lexes" $
      tokenize source `shouldBe`
      Right
        [ "\"module\""
        , "\"Main\""
        , "\"where\""
        , "\"import\""
        , "qualifier"
        , "qualifier"
        , "qualifier"
        , "\"Console\""
        , "\"main\""
        , "="
        , "\"log\""
        , "\"Hello, World!\""
        ]
    it "parses" $ do
      let (Right mod) = parse source
      let (PS.Module srcSpan comments modName declarations declarRefs) = mod
      -- srcSpan `shouldBe` undefined
      comments `shouldBe` []
      -- modName `shouldBe` undefined
      -- declarations `shouldBe` undefined -- We don't have Eq here
      declarRefs `shouldBe` Nothing
