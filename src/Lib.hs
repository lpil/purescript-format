module Lib where

import Control.Arrow ((>>>))
import Data.String.Here
import qualified Language.PureScript as PS
import Protolude
import qualified Text.Parsec as P (ParseError)

greet :: Text -> Text
greet name = "Hello, " <> name <> "!"

tokenize :: Text -> Either P.ParseError [Text]
tokenize = PS.lex "file.purs" >>> fmap (map show)

parse :: Text -> Either P.ParseError PS.Module
parse source =
  PS.lex "file.purs" source >>= PS.runTokenParser "file.purs" PS.parseModule

source :: Text
source =
  [here|
module Main where

import Control.Monad.Eff.Console

main = log "Hello, World!"
|]
-- Module
--   (SourceSpan
--    { spanName = "file.purs"
--    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
--    , spanEnd = SourcePos {sourcePosLine = 5, sourcePosColumn = 12}
--    })
--   []
--   (ModuleName [ProperName {runProperName = "Main"}])
--   [ ImportDeclaration
--       ( SourceSpan
--         { spanName = "file.purs"
--         , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}
--         , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 33}
--         }
--       , [])
--       (ModuleName
--          [ ProperName {runProperName = "Control"}
--          , ProperName {runProperName = "Monad"}
--          , ProperName {runProperName = "Eff"}
--          , ProperName {runProperName = "Console"}
--          ])
--       Implicit
--       Nothing
--   , ValueDeclaration
--       (ValueDeclarationData
--        { valdeclSourceAnn =
--            ( SourceSpan
--              { spanName = "file.purs"
--              , spanStart = SourcePos {sourcePosLine = 5, sourcePosColumn = 1}
--              , spanEnd = SourcePos {sourcePosLine = 5, sourcePosColumn = 12}
--              }
--            , [])
--        , valdeclIdent = Ident "main"
--        , valdeclName = Public
--        , valdeclBinders = []
--        , valdeclExpression =
--            [ GuardedExpr
--                []
--                (PositionedValue
--                   (SourceSpan
--                    { spanName = "file.purs"
--                    , spanStart =
--                        SourcePos {sourcePosLine = 5, sourcePosColumn = 8}
--                    , spanEnd =
--                        SourcePos {sourcePosLine = 5, sourcePosColumn = 12}
--                    })
--                   []
--                   (PositionedValue
--                      (SourceSpan
--                       { spanName = "file.purs"
--                       , spanStart =
--                           SourcePos {sourcePosLine = 5, sourcePosColumn = 8}
--                       , spanEnd =
--                           SourcePos {sourcePosLine = 5, sourcePosColumn = 12}
--                       })
--                      []
--                      (App
--                         (PositionedValue
--                            (SourceSpan
--                             { spanName = "file.purs"
--                             , spanStart =
--                                 SourcePos
--                                 {sourcePosLine = 5, sourcePosColumn = 8}
--                             , spanEnd =
--                                 SourcePos
--                                 {sourcePosLine = 5, sourcePosColumn = 11}
--                             })
--                            []
--                            (Var (Qualified Nothing (Ident "log"))))
--                         (PositionedValue
--                            (SourceSpan
--                             { spanName = "file.purs"
--                             , spanStart =
--                                 SourcePos
--                                 {sourcePosLine = 5, sourcePosColumn = 12}
--                             , spanEnd =
--                                 SourcePos
--                                 {sourcePosLine = 5, sourcePosColumn = 12}
--                             })
--                            []
--                            (Literal (StringLiteral "Hello, World!"))))))
--            ]
--        })
--   ]
--   Nothing
