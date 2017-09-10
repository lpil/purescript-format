module Language.PureScript.Format
  ( format
  , formatWith
  , defaultOptions
  , Options(..)
  ) where

import Control.Category ((>>>))
import qualified Language.PureScript as PS
import Protolude
import qualified Text.Parsec as Parsec
import Text.PrettyPrint.Leijen.Text as PrettyPrint

data Options = Options
  { maxWidth :: Int
  , filename :: FilePath
  }

{- Hey, take this. It will come in handy.
   https://hackage.haskell.org/package/wl-pprint-text-1.1.1.0/docs/Text-PrettyPrint-Leijen-Text.html
-}
{-| Left to right function application.
-}
(|>) :: a -> (a -> c) -> c
(|>) = flip ($)

infixl 9 |>

{-| Some sensible defaults for formatting PS code.
-}
defaultOptions :: Options
defaultOptions = Options {maxWidth = 80, filename = "nofile"}

{-| Pretty print PS source code.
-}
format :: Text -> Either Parsec.ParseError Text
format = do
  formatWith defaultOptions

{-| Pretty print PS source code using the options provided.
-}
formatWith :: Options -> Text -> Either Parsec.ParseError Text
formatWith options source = do
  module' <- parse (filename options) source
  Module module' |> pretty |> renderPretty 0.9 (maxWidth options) |>
    displayTStrict |>
    pure

{-| A Module of PS code.
-}
newtype Module =
  Module PS.Module

instance Pretty Module where
  pretty (Module (PS.Module _ _ moduleName declarations _)) =
    mconcat [prettyModuleHeader moduleName, prettyDeclarations declarations]

prettyModuleHeader :: PS.ModuleName -> Doc
prettyModuleHeader name =
  mconcat ["module ", pretty (ModuleName name), " where"]

prettyDeclarations :: [PS.Declaration] -> Doc
prettyDeclarations [] = mempty
prettyDeclarations declarations =
  let declarationDocs =
        declarations |> map (Declaration >>> pretty) |> intersperse linebreak
  in mconcat $ linebreak : linebreak : declarationDocs

{-| The module statement, module name, and exports.
-}
newtype ModuleName =
  ModuleName PS.ModuleName

instance Pretty ModuleName where
  pretty (ModuleName (PS.ModuleName names)) =
    let nameDoc = PS.runProperName >>> fromStrict >>> text
    in names |> map nameDoc |> intersperse dot |> mconcat

{-| A top level declaration, such as an import or a function definition.
-}
newtype Declaration =
  Declaration PS.Declaration

-- purescript/src/Language/PureScript/AST/Declarations.hs
instance Pretty Declaration where
  pretty (Declaration dec) =
    case dec of
      PS.ImportDeclaration _ name type' qualifier ->
        prettyImport name type' qualifier
      PS.DataDeclaration _ _ _ _ _ -> undefined
      PS.DataBindingGroupDeclaration _ -> undefined
      PS.TypeSynonymDeclaration _ _ _ _ -> undefined
      PS.TypeDeclaration _ -> undefined
      PS.ValueDeclaration _ -> undefined
      PS.BoundValueDeclaration _ _ _ -> undefined
      PS.BindingGroupDeclaration _ -> undefined
      PS.ExternDeclaration _ _ _ -> undefined
      PS.ExternDataDeclaration _ _ _ -> undefined
      PS.ExternKindDeclaration _ _ -> undefined
      PS.FixityDeclaration _ _ -> undefined
      PS.TypeClassDeclaration _ _ _ _ _ _ -> undefined
      PS.TypeInstanceDeclaration _ _ _ _ _ _ -> undefined

prettyImport :: PS.ModuleName
             -> PS.ImportDeclarationType
             -> Maybe PS.ModuleName
             -> Doc
prettyImport name PS.Implicit Nothing = "import " <> pretty (ModuleName name)
prettyImport _ _ _ = undefined

{-| Parse a module of PS source code.
-}
parse :: FilePath -> Text -> Either Parsec.ParseError PS.Module
parse path source = tokenize path source >>= parseTokens path
  where
    tokenize filepath = PS.lex filepath
    parseTokens filepath = PS.runTokenParser filepath PS.parseModule
