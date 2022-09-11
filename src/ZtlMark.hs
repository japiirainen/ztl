module ZtlMark (
  ZtlMark (..),
  ZtlMarkError (..),
  parse,
  Extension,
  render,
  projectYaml,
) where

import Data.Aeson
import ZtlMark.Parser (ZtlMarkError (..), parse)
import ZtlMark.Render (render)
import ZtlMark.Type

-- | Extract contents of an optional YAML block that may have been parsed.
projectYaml :: ZtlMark -> Maybe Value
projectYaml = ztlMarkYaml
