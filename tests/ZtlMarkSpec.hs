module ZtlMarkSpec (spec) where

import Data.Aeson
import Test.Hspec
import Test.Hspec.Megaparsec
import TestUtils
import Text.Megaparsec (ErrorFancy (..), Stream)
import ZtlMark

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

  describe "YAML block" $ do
    context "when doc does not contain a YAML section" $
      it "returns Nothing" $ do
        doc <- mkDoc "No YAML block."
        ZtlMark.projectYaml doc `shouldBe` Nothing

    context "when document contains a YAML block" $ do
      let expected =
            object
              [ "x" .= Number 69
              , "y" .= Number 420
              ]
      it "returns the YAML section (1)" $ do
        doc <- mkDoc "---\nx: 69\ny: 420\n---\nHere we go."
        ZtlMark.projectYaml doc `shouldBe` Just expected
      it "returns the YAML section (2)" $ do
        doc <- mkDoc "---\nx: 69\ny: 420\n---\n\n"
        ZtlMark.projectYaml doc `shouldBe` Just expected

    context "when it is invalid" $ do
      let mappingError =
            fancy . ErrorCustom . YamlParseError $
              "mapping values are not allowed in this context"
      it "signals correct parse error" $
        let s = "---\nx: 100\ny: x:\n---\nHere we go."
         in s ~-> errFancy 15 mappingError
      it "does not choke and can report more parse errors" $
        let s = "---\nx: 100\ny: x:\n---\nHere we *go."
         in s
              ~~-> [ errFancy 15 mappingError
                   , err 33 (ueib <> etok '*' <> eic)
                   ]

-- | Unexpected end of inline block.
ueib :: Stream s => ET s
ueib = ulabel "end of inline block"

-- | Expecting inline content.
eic :: Stream s => ET s
eic = elabel "inline content"
