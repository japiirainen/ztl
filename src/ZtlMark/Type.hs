{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module ZtlMark.Type (
  ZtlMark (..),
  Extension (..),
  Render (..),
  Bni,
  Block (..),
  Inline (..),
  Ois,
  mkOisInternal,
  getOis,
) where

import Control.DeepSeq
import Data.Aeson
import Data.Data (Data)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Lucid

data ZtlMark = ZtlMark
  { ztlMarkYaml :: Maybe Value
  , ztlMarkBlocks :: [Bni]
  , ztlMarkExtensions :: Extension
  }

instance NFData ZtlMark where
  rnf ZtlMark {..} = rnf ztlMarkYaml `seq` rnf ztlMarkBlocks

instance Show ZtlMark where
  show _ = "ZtlMark {..}"

data Extension = Extension
  { extBlockTrans :: Endo Bni
  , extBlockRender :: Render (Block (Ois, Html ()))
  , extInlineTrans :: Endo Inline
  , extInlineRender :: Render Inline
  }

instance Semigroup Extension where
  e1 <> e2 =
    Extension
      { extBlockTrans = on (<>) extBlockTrans e1 e2
      , extBlockRender = on (<>) extBlockRender e1 e2
      , extInlineTrans = on (<>) extInlineTrans e1 e2
      , extInlineRender = on (<>) extInlineRender e1 e2
      }

instance Monoid Extension where
  mempty =
    Extension
      { extBlockTrans = mempty
      , extBlockRender = mempty
      , extInlineTrans = mempty
      , extInlineRender = mempty
      }
  mappend = (<>)

newtype Render a = Render
  {runRender :: (a -> Html ()) -> a -> Html ()}

instance Semigroup (Render a) where
  Render f <> Render g = Render (f . g)

instance Monoid (Render a) where
  mempty = Render id
  mappend = (<>)

type Bni = Block (NonEmpty Inline)

data Block a
  = Heading1 a
  | Paragraph a
  | Naked a
  | Blockquote [Block a]
  deriving stock (Show, Eq, Ord)
  deriving (Data, Typeable, Generic, Functor, Foldable)

instance NFData a => NFData (Block a)

data Inline
  = Plain Text
  | LineBreak
  | Strong (NonEmpty Inline)
  deriving stock (Show, Eq, Ord)
  deriving (Data, Typeable, Generic)

instance NFData Inline

newtype Ois = Ois (NonEmpty Inline)

mkOisInternal :: NonEmpty Inline -> Ois
mkOisInternal = Ois

getOis :: Ois -> NonEmpty Inline
getOis (Ois is) = is
