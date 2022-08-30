{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module ZtlMark.Parser.Internal.Type (
  -- * Block-level parser state
  BlockState,
  initialBlockState,
  bstAllowNaked,
  bstRefLevel,
  bstDefs,

  -- * Inline-level parser state
  InlineState,
  initialInlineState,
  istLastChar,
  istAllowEmpty,
  istAllowLinks,
  istAllowImages,
  istDefs,
  Isp (..),
  CharType (..),

  -- * Reference and footnote definitions
  Defs,
  referenceDefs,
  DefLabel,
  mkDefLabel,
  unDefLabel,

  -- * Other
  ZtlMarkError (..),
) where

import Control.DeepSeq
import Data.CaseInsensitive (CI)
import Data.Data (Data)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lens.Micro.TH
import Text.Megaparsec
import Text.URI (URI)

import Data.CaseInsensitive qualified as CI
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T

{- | Block-level parser state.
 Eg. # hello
       world
-}
data BlockState = BlockState
  { _bstAllowNaked :: Bool
  , _bstRefLevel :: Pos
  , _bstDefs :: Defs
  }

initialBlockState :: BlockState
initialBlockState =
  BlockState
    { _bstAllowNaked = False
    , _bstRefLevel = pos1
    , _bstDefs = emptyDefs
    }

{- | Inline-level parser state.
 Eg. # hello *string*.
-}
data InlineState = InlineState
  { -- | Type of the last encountered character
    _istLastChar :: !CharType
  , -- | Wheather to allow empty inlines
    _istAllowEmpty :: Bool
  , _istAllowLinks :: Bool
  , _istAllowImages :: Bool
  , -- | Reference link definitions
    _istDefs :: Defs
  }

initialInlineState :: InlineState
initialInlineState =
  InlineState
    { _istLastChar = SpaceChar
    , _istAllowEmpty = True
    , _istAllowLinks = True
    , _istAllowImages = True
    , _istDefs = emptyDefs
    }

-- | 'Inline' source pending parsing.
data Isp
  = -- | We have an inline source pending parsing
    IspSpan Int Text
  | -- | We should just return this parse error
    IspError (ParseError Text ZtlMarkError)
  deriving stock (Eq, Show)

-- | Type of the last seen character
data CharType
  = SpaceChar
  | PunctChar
  | OtherChar
  deriving stock (Eq, Ord, Show)

-- | An opaque container for reference and footnote definitions.
newtype Defs = Defs
  { -- | Reference definitions containing a 'URI' and optionally title
    _referenceDefs :: HashMap DefLabel (URI, Maybe Text)
  }

-- | An opaque type for definition label.
newtype DefLabel = DefLabel (CI Text)
  deriving newtype (Eq, Ord, Hashable)

emptyDefs :: Defs
emptyDefs =
  Defs
    { _referenceDefs = HM.empty
    }

-- | Smart constructor for the 'DefLabel' type.
mkDefLabel :: Text -> DefLabel
mkDefLabel = DefLabel . CI.mk . T.unwords . T.words

-- | Extract 'Text' value from a 'DefLabel'.
unDefLabel :: DefLabel -> Text
unDefLabel (DefLabel x) = CI.original x

data ZtlMarkError
  = -- | YAML error that occurred during YAML block parsing
    YamlParseError String
  | -- | Unknown HTML5 entity name
    UnknownHtmlEntityName Text
  deriving stock (Eq, Ord, Show, Read, Typeable)
  deriving (Data, Generic)

instance ShowErrorComponent ZtlMarkError where
  showErrorComponent = \case
    YamlParseError str ->
      "YAML parse error: " ++ str
    UnknownHtmlEntityName name ->
      "unknown HTML5 entity name: \"" ++ T.unpack name ++ "\""

instance NFData ZtlMarkError

makeLenses ''BlockState
makeLenses ''InlineState
makeLenses ''Defs
