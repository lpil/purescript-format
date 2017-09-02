module Language.PureScript.Format
  ( format
  ) where

import Protolude

format :: Int -> Text -> Text
format _ source = source
