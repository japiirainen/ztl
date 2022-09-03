module ZtlMark.Util where

import Data.Char (isAlphaNum, isSpace)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as T
import Text.URI (URI (..))
import Text.URI qualified as URI
import ZtlMark.Type

asPlainText :: NonEmpty Inline -> Text
asPlainText = foldMap $ \case
  Plain txt -> txt
  LineBreak -> "\n"
  Strong xs -> asPlainText xs

headerId :: NonEmpty Inline -> Text
headerId =
  T.intercalate "-"
    . T.words
    . T.filter (\x -> isAlphaNum x || isSpace x)
    . T.toLower
    . asPlainText

headerFragment :: Text -> URI
headerFragment fragment =
  URI
    { uriScheme = Nothing
    , uriAuthority = Left False
    , uriPath = Nothing
    , uriQuery = []
    , uriFragment = URI.mkFragment fragment
    }
