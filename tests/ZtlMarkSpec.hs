module ZtlMarkSpec (spec) where

import Test.Hspec
import TestUtils

spec :: Spec
spec = parallel $ do
  describe "parse and render" $ do
    context "paragraphs" $ do
      it "CM182" $
        "aaa\n\nbbb"
          ==-> "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM183" $
        "aaa\nbbb\n\nccc\nddd"
          ==-> "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"
      it "CM184" $
        "aaa\n\n\nbbb"
          ==-> "<p>aaa</p>\n<p>bbb</p>\n"
      it "CM185" $
        "  aaa\n bbb"
          ==-> "<p>aaa\nbbb</p>\n"
      it "CM186" $
        "aaa\n             bbb\n                                       ccc"
          ==-> "<p>aaa\nbbb\nccc</p>\n"
      it "CM187" $
        "   aaa\nbbb" ==-> "<p>aaa\nbbb</p>\n"
