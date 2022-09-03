module ZtlMark.Render (render) where

import Control.Arrow
import Data.Function (fix)
import Lucid
import ZtlMark.Trans
import ZtlMark.Type
import ZtlMark.Util

render :: ZtlMark -> Html ()
render ZtlMark {..} =
  mapM_ renderBlock ztlMarkBlocks
  where
    Extension {..} = ztlMarkExtensions
    renderBlock =
      applyBlockRender extBlockRender
        . fmap renderInlines
        . applyBlockTrans extBlockTrans
    renderInlines =
      (mkOisInternal &&& mapM_ (applyInlineRender extInlineRender))
        . fmap (applyInlineTrans extInlineTrans)

applyBlockRender ::
  Render (Block (Ois, Html ())) ->
  Block (Ois, Html ()) ->
  Html ()
applyBlockRender r = fix (runRender r . defaultBlockRender)

defaultBlockRender ::
  (Block (Ois, Html ()) -> Html ()) ->
  Block (Ois, Html ()) ->
  Html ()
defaultBlockRender blockRender = \case
  Heading1 (h, html) ->
    h1_ (mkId h) html >> newline
  Paragraph (_, html) ->
    p_ html >> newline
  Naked (_, html) ->
    html >> newline
  Blockquote bs -> do
    blockquote_ (newline <* mapM_ blockRender bs)
    newline
  where
    mkId ois = [(id_ . headerId . getOis) ois]

applyInlineRender :: Render Inline -> Inline -> Html ()
applyInlineRender r = fix (runRender r . defaultInlineRender)

defaultInlineRender ::
  (Inline -> Html ()) ->
  Inline ->
  Html ()
defaultInlineRender inlineRender = \case
  Plain txt ->
    toHtml txt
  LineBreak ->
    br_ [] >> newline
  Strong inner ->
    strong_ (mapM_ inlineRender inner)

newline :: Html ()
newline = "\n"
