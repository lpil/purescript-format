module Language.PureScript.Format
  ( format
  , formatWith
  , defaultOptions
  , Options(..)
  ) where

import Control.Category ((>>>))
import qualified Language.PureScript as PS
import Protolude hiding (group)
import qualified Text.Parsec as Parsec

-- https://hackage.haskell.org/package/wl-pprint-text-1.1.1.0/docs/Text-PrettyPrint-Leijen-Text.html
import Text.PrettyPrint.Leijen.Text as PrettyPrint

data Options = Options
  { maxWidth :: Int
  , filename :: FilePath
  }

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
  pure (formatRender module')
  where
    formatRender =
      prettyModule >>> renderPretty 0.9 (maxWidth options) >>> displayTStrict

prettyModule :: PS.Module -> Doc
prettyModule (PS.Module _ _ moduleName declarations _) =
  mconcat [prettyModuleHeader moduleName, prettyDeclarations declarations]

prettyModuleHeader :: PS.ModuleName -> Doc
prettyModuleHeader name = mconcat ["module ", prettyModuleName name, " where"]

prettyModuleName :: PS.ModuleName -> Doc
prettyModuleName (PS.ModuleName names) =
  let nameDoc = PS.runProperName >>> fromStrict >>> text
  in names |> map nameDoc |> intersperse dot |> mconcat

prettyDeclarations :: [PS.Declaration] -> Doc
prettyDeclarations [] = mempty
prettyDeclarations declarations =
  let declarationDocs =
        declarations |> map prettyDeclaration |> intersperse linebreak
  in mconcat $ linebreak : linebreak : declarationDocs

{-| A top level declaration, such as an import or a function definition.

purescript/src/Language/PureScript/AST/Declarations.hs
-}
prettyDeclaration :: PS.Declaration -> Doc
prettyDeclaration (PS.ImportDeclaration _ name type' qualifier) =
  prettyImport name type' qualifier
prettyDeclaration (PS.DataDeclaration _ _ _ _ _) = undefined
prettyDeclaration (PS.DataBindingGroupDeclaration _) = undefined
prettyDeclaration (PS.TypeSynonymDeclaration _ _ _ _) = undefined
prettyDeclaration (PS.TypeDeclaration _) = undefined
prettyDeclaration (PS.ValueDeclaration _) = undefined
prettyDeclaration (PS.BoundValueDeclaration _ _ _) = undefined
prettyDeclaration (PS.BindingGroupDeclaration _) = undefined
prettyDeclaration (PS.ExternDeclaration _ _ _) = undefined
prettyDeclaration (PS.ExternDataDeclaration _ _ _) = undefined
prettyDeclaration (PS.ExternKindDeclaration _ _) = undefined
prettyDeclaration (PS.FixityDeclaration _ _) = undefined
prettyDeclaration (PS.TypeClassDeclaration _ _ _ _ _ _) = undefined
prettyDeclaration (PS.TypeInstanceDeclaration _ _ _ _ _ _) = undefined

prettyImport :: PS.ModuleName
             -> PS.ImportDeclarationType
             -> Maybe PS.ModuleName
             -> Doc
prettyImport name PS.Implicit Nothing = "import " <> prettyModuleName name
prettyImport name (PS.Explicit defRefs) Nothing =
  let header = prettyImport name PS.Implicit Nothing
      refsDocs =
        map prettyDeclarationRef defRefs |> intersperse ("," <> softline)
      refsDoc = refsDocs |> mconcat |> nest 1 |> parens
  in (header </> refsDoc) |> nest 7 |> group
prettyImport _ _ _ = undefined

prettyDeclarationRef :: PS.DeclarationRef -> Doc
prettyDeclarationRef (PS.ValueRef _ (PS.Ident ident)) = text (fromStrict ident)
prettyDeclarationRef _ = undefined

{-| Parse a module of PS source code.
-}
parse :: FilePath -> Text -> Either Parsec.ParseError PS.Module
parse path source = tokenize path source >>= parseTokens path
  where
    tokenize filepath = PS.lex filepath
    parseTokens filepath = PS.runTokenParser filepath PS.parseModule
