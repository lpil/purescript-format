module Language.PureScript.Format
  ( format
  , formatWith
  , defaultOptions
  , Options(..)
  ) where

-- TODO: Define formatWith or formatWithFile so more info can be specified.
import Control.Category ((>>>))
import qualified Language.PureScript as PureScript
import Protolude
import qualified Text.Parsec as Parsec
import Text.PrettyPrint.Leijen.Text as PrettyPrint

data Options = Options
  { maxWidth :: Int
  , filename :: FilePath
  }

{- Hey, take this. It will come in handy.
   https://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.1/docs/Text-PrettyPrint-ANSI-Leijen.html
-}
{-| Left to right function application.
-}
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

{-| Some sensible defaults for formatting PureScript code.
-}
defaultOptions :: Options
defaultOptions = Options {maxWidth = 80, filename = "nofile"}

{-| Pretty print PureScript source code.
-}
format :: Text -> Either Parsec.ParseError Text
format = do
  formatWith defaultOptions

{-| Pretty print PureScript source code using the options provided.
-}
formatWith :: Options -> Text -> Either Parsec.ParseError Text
formatWith options source = do
  module' <- parse (filename options) source
  Module module' |> pretty |> renderPretty 0.9 (maxWidth options) |>
    displayTStrict |>
    pure

{- Wrappers for PureScript types so we can define the Pretty class
   for them without GHC complaining about orphan instances.
-}
newtype Module =
  Module PureScript.Module

instance Pretty Module where
  pretty (Module (PureScript.Module _ _ modName _ _)) =
    pretty (ModuleName modName)

newtype ModuleName =
  ModuleName PureScript.ModuleName

instance Pretty ModuleName where
  pretty (ModuleName (PureScript.ModuleName names)) =
    let nameDoc = PureScript.runProperName >>> fromStrict >>> text
        modName = names |> map nameDoc |> intersperse dot |> mconcat
    in mconcat ["module ", modName, " where"]

{-| Parse a module of PureScript source code.
-}
parse :: FilePath -> Text -> Either Parsec.ParseError PureScript.Module
parse path source = tokenize path source >>= parseTokens path
  where
    tokenize filepath = PureScript.lex filepath
    parseTokens filepath =
      PureScript.runTokenParser filepath PureScript.parseModule
