module Lib
  ( greet
  ) where

import Protolude

greet :: Text -> Text
greet name = "Hello, " <> name <> "!"
