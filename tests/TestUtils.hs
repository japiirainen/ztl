module TestUtils where

import Control.Monad
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Lucid qualified as L
import Test.Hspec
import Text.Megaparsec
import ZtlMark (ZtlMark, ZtlMarkError)
import ZtlMark qualified

mkDoc :: Text -> IO ZtlMark
mkDoc input =
  case ZtlMark.parse "" input of
    Left bundle -> do
      expectationFailure $
        "while parsing a document, parse error(s) occurred:\n"
          <> errorBundlePretty bundle
      undefined
    Right x -> pure x

toText :: ZtlMark -> Text
toText = TL.toStrict . L.renderText . ZtlMark.render

{- | Create an expectation that parser should fail producing
 a certain collection of 'ParseError's.
-}
infix 2 ~~->

(~~->) :: Text -> [ParseError Text ZtlMarkError] -> Expectation
input ~~-> es =
  case ZtlMark.parse "" input of
    Left bundle ->
      unless (bundle == bundle') . expectationFailure $
        "\nthe parser is expected to fail with:\n\n"
          <> errorBundlePretty bundle'
          <> "\nbut it failed with:\n\n"
          <> errorBundlePretty bundle
    Right x ->
      expectationFailure $
        "the parser is expected to fail, but it parsed: " <> show (toText x)
  where
    bundle' =
      ParseErrorBundle
        { bundleErrors = NE.fromList es
        , bundlePosState =
            PosState
              { pstateInput = input
              , pstateOffset = 0
              , pstateSourcePos = initialPos ""
              , pstateTabWidth = mkPos 4
              , pstateLinePrefix = ""
              }
        }

-- | The same as @('~~->')@, but expects only one parse error.
infix 2 ~->

(~->) :: Text -> ParseError Text ZtlMarkError -> Expectation
input ~-> err = input ~~-> [err]

{- | Test parser and render by specifying input for parser and expected
 output of render.
-}
infix 2 =->

(=->) :: Text -> Text -> Expectation
input =-> expected =
  case ZtlMark.parse "" input of
    Left bundle ->
      expectationFailure $
        "the parser is expected to succeed, but it failed with:\n"
          <> errorBundlePretty bundle
    Right actual -> toText actual `shouldBe` expected

{- | Just like @('=->')@, but also appends newline to given input and tries
 with that as well.
-}
infix 9 ==->

(==->) :: Text -> Text -> Expectation
input ==-> expected = do
  input =-> expected
  mappend input "\n" =-> expected
