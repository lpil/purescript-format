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
  names |> map prettyProperName |> intersperse dot |> mconcat

prettyProperName :: PS.ProperName a -> Doc
prettyProperName (PS.ProperName name) = text (fromStrict name)

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
prettyDeclaration x@(PS.DataDeclaration _ _ _ _ _) = unimplemented x
prettyDeclaration x@(PS.DataBindingGroupDeclaration _) = unimplemented x
prettyDeclaration x@(PS.TypeSynonymDeclaration _ _ _ _) = unimplemented x
prettyDeclaration x@(PS.TypeDeclaration _) = unimplemented x
prettyDeclaration x@(PS.ValueDeclaration _) = unimplemented x
prettyDeclaration x@(PS.BoundValueDeclaration _ _ _) = unimplemented x
prettyDeclaration x@(PS.BindingGroupDeclaration _) = unimplemented x
prettyDeclaration x@(PS.ExternDeclaration _ _ _) = unimplemented x
prettyDeclaration x@(PS.ExternDataDeclaration _ _ _) = unimplemented x
prettyDeclaration x@(PS.ExternKindDeclaration _ _) = unimplemented x
prettyDeclaration x@(PS.FixityDeclaration _ _) = unimplemented x
prettyDeclaration x@(PS.TypeClassDeclaration _ _ _ _ _ _) = unimplemented x
prettyDeclaration x@(PS.TypeInstanceDeclaration _ _ _ _ _ _) = unimplemented x

prettyImport :: PS.ModuleName
             -> PS.ImportDeclarationType
             -> Maybe PS.ModuleName
             -> Doc
prettyImport name PS.Implicit Nothing = "import " <> prettyModuleName name
prettyImport name (PS.Explicit defRefs) Nothing =
  let header = prettyImport name PS.Implicit Nothing
      refsDoc =
        map prettyDeclarationRef defRefs |> intersperse ("," <> softline) |>
        mconcat |>
        nest 8 |>
        group |>
        parens
  in (header <> softline |> nest 7 |> group) <> refsDoc |> group
prettyImport a b c = unimplemented (a, b, c)

prettyDeclarationRef :: PS.DeclarationRef -> Doc
prettyDeclarationRef (PS.ValueRef _ (PS.Ident ident)) = text (fromStrict ident)
prettyDeclarationRef x@(PS.ValueRef _ (PS.GenIdent _ _)) = unimplemented x
prettyDeclarationRef (PS.ValueOpRef _ (PS.OpName ident)) =
  ident |> fromStrict |> text |> parens
prettyDeclarationRef (PS.TypeRef _ name (Just [])) = prettyProperName name
prettyDeclarationRef (PS.TypeRef _ name (Just names)) =
  prettyProperName name <>
  (map prettyProperName names |> intersperse ("," <> space) |> mconcat |> parens)
prettyDeclarationRef x@(PS.TypeRef _ _ _) = unimplemented x
prettyDeclarationRef x@(PS.TypeOpRef _ _) = unimplemented x
prettyDeclarationRef x@(PS.TypeClassRef _ _) = unimplemented x
prettyDeclarationRef x@(PS.ModuleRef _ _) = unimplemented x
prettyDeclarationRef x@(PS.TypeInstanceRef _ _) = unimplemented x
prettyDeclarationRef x@(PS.KindRef _ _) = unimplemented x
prettyDeclarationRef x@(PS.ReExportRef _ _ _) = unimplemented x

{-| Parse a module of PS source code.
-}
parse :: FilePath -> Text -> Either Parsec.ParseError PS.Module
parse path source = tokenize path source >>= parseTokens path
  where
    tokenize filepath = PS.lex filepath
    parseTokens filepath = PS.runTokenParser filepath PS.parseModule

unimplemented
  :: Show a
  => a -> b
unimplemented x = error ("function clause not implemented for " <> show x)
