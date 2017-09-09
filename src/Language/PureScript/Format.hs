module Language.PureScript.Format
  ( format
  ) where

-- TODO: Define formatWith or formatWithFile so more info can be specified.
import Control.Category ((>>>))
import qualified Language.PureScript as PureScript
import Protolude
import qualified Text.Parsec as Parsec
import Text.PrettyPrint.Leijen.Text as PrettyPrint

{- Hey, take this. It will come in handy.
   https://hackage.haskell.org/package/ansi-wl-pprint-0.6.8.1/docs/Text-PrettyPrint-ANSI-Leijen.html
-}
{-| Left to right function application.
-}
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

{-| Pretty print PureScript source code.
-}
format :: Int -> Text -> Either Parsec.ParseError Text
format maxWidth source = do
  module' <- parse source
  Module module' |> pretty |> renderPretty 0.9 maxWidth |> displayTStrict |>
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
parse :: Text -> Either Parsec.ParseError PureScript.Module
parse source = tokenize source >>= parseTokens
  where
    tokenize = PureScript.lex "nofile"
    parseTokens = PureScript.runTokenParser "nofile" PureScript.parseModule
