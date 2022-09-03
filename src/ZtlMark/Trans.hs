module ZtlMark.Trans (
  applyBlockTrans,
  applyInlineTrans,
) where

import Data.Monoid hiding ((<>))
import ZtlMark.Type

applyBlockTrans :: Endo Bni -> Bni -> Bni
applyBlockTrans trans@(Endo f) = \case
  Blockquote xs -> f (Blockquote (s xs))
  other -> f other
  where
    s = fmap (applyBlockTrans trans)

applyInlineTrans :: Endo Inline -> Inline -> Inline
applyInlineTrans trans@(Endo f) = \case
  Strong xs -> f (Strong (s xs))
  other -> f other
  where
    s = fmap (applyInlineTrans trans)
