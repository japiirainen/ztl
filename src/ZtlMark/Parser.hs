module ZtlMark.Parser (
  parse,
  ZtlMarkError (..),
) where

import Control.Applicative (Alternative)
import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Text.Megaparsec hiding (State (..), parse)
import Text.Megaparsec.Char hiding (eol)
import ZtlMark.Parser.Internal
import ZtlMark.Type

import qualified Data.Aeson as Aeson
import qualified Data.DList as DList
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import qualified Data.Set as E
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Yaml
import qualified Text.Megaparsec.Char.Lexer as L

parse :: FilePath -> Text -> Either (ParseErrorBundle Text ZtlMarkError) ZtlMark
parse file input =
  case runBParser pZtlMark file input of
    Left bundle -> Left bundle
    Right ((yaml, rawBlocks), defs) ->
      let parsed = doInline <$> rawBlocks
          doInline =
            fmap $
              first (replaceEof "end of inline block")
                . runIParser defs pInlinesTop
          e2p = either DList.singleton (const DList.empty)
          fromRight (Right x) = x
          fromRight _ = error "This is not possible!!!"
       in case NE.nonEmpty . DList.toList $ foldMap (foldMap e2p) parsed of
            Nothing ->
              Right
                ZtlMark
                  { ztlMarkYaml = yaml
                  , ztlMarkBlocks = fmap fromRight <$> parsed
                  , ztlMarkExtensions = mempty
                  }
            Just es ->
              Left
                ParseErrorBundle
                  { bundleErrors = es
                  , bundlePosState =
                      PosState
                        { pstateInput = input
                        , pstateOffset = 0
                        , pstateSourcePos = initialPos file
                        , pstateTabWidth = mkPos 4
                        , pstateLinePrefix = ""
                        }
                  }

pZtlMark :: BParser (Maybe Aeson.Value, [Block Isp])
pZtlMark = do
  meyaml <- optional pYamlBlock
  blocks <- pBlocks
  eof
  pure $ case meyaml of
    Nothing -> (Nothing, blocks)
    Just (Left (o, e)) -> (Nothing, prependErr o (YamlParseError e) blocks)
    Just (Right yaml) -> (Just yaml, blocks)

pBlocks :: BParser [Block Isp]
pBlocks = catMaybes <$> many pBlock

pBlock :: BParser (Maybe (Block Isp))
pBlock = do
  sc
  rlevel <- refLevel
  alevel <- L.indentLevel
  done <- atEnd
  if done || alevel < rlevel
    then empty
    else
      choice
        [Just <$> pParagraph]

pParagraph :: BParser (Block Isp)
pParagraph = do
  startOffset <- getOffset
  allowNaked <- isNakedAllowed
  rlevel <- refLevel
  let go ls = do
        l <- lookAhead (option "" nonEmptyLine)
        broken <- succeeds . lookAhead . try $ do
          sc
          alevel <- L.indentLevel
          guard (alevel < ilevel rlevel)
          unless (alevel < rlevel) . choice $
            [void (char '>')]
        if isBlank l
          then pure (ls, Paragraph)
          else
            if broken
              then return (ls, Naked)
              else do
                void nonEmptyLine
                continue <- eol'
                let ls' = ls . (l :)
                if continue
                  then go ls'
                  else pure (ls', Naked)
  l <- nonEmptyLine
  continue <- eol'
  (ls, toBlock) <-
    if continue
      then go id
      else pure (id, Naked)
  (if allowNaked then toBlock else Paragraph)
    (IspSpan startOffset (assembleParagraph (l : ls [])))
    <$ sc

pYamlBlock :: BParser (Either (Int, String) Aeson.Value)
pYamlBlock = do
  string "---" *> sc' *> eol
  doffset <- getOffset
  ls <- go id <*> ([] <$ sc)
  pure $ decodeYaml ls doffset
  where
    go acc = do
      l <- takeWhileP Nothing notNewline
      void (optional eol)
      e <- atEnd
      if e || T.stripEnd l == "---"
        then pure acc
        else go (acc . (l :))

decodeYaml :: [T.Text] -> Int -> Either (Int, String) Aeson.Value
decodeYaml ls doffset =
  case (Yaml.decodeEither' . TE.encodeUtf8 . T.intercalate "\n") ls of
    Left err' ->
      let (moffset, err) = splitYamlError err'
       in Left (maybe doffset (+ doffset) moffset, err)
    Right v -> Right v

splitYamlError ::
  Yaml.ParseException ->
  (Maybe Int, String)
splitYamlError = \case
  Yaml.NonScalarKey -> (Nothing, "non scalar key")
  Yaml.UnknownAlias anchor -> (Nothing, "unknown alias \"" ++ anchor ++ "\"")
  Yaml.UnexpectedEvent exptd unexptd ->
    ( Nothing
    , "unexpected event: expected " ++ show exptd
        ++ ", but received "
        ++ show unexptd
    )
  Yaml.InvalidYaml myerror -> case myerror of
    Nothing -> (Nothing, "unspecified error")
    Just yerror -> case yerror of
      Yaml.YamlException s -> (Nothing, s)
      Yaml.YamlParseException problem context mark ->
        ( Just (Yaml.yamlIndex mark)
        , case context of
            "" -> problem
            _ -> context ++ ", " ++ problem
        )
  Yaml.AesonException s -> (Nothing, s)
  Yaml.OtherParseException exc -> (Nothing, show exc)
  Yaml.NonStringKeyAlias anchor value ->
    ( Nothing
    , "non-string key alias; anchor name: " ++ anchor
        ++ ", value: "
        ++ show value
    )
  Yaml.CyclicIncludes -> (Nothing, "cyclic includes")
  Yaml.LoadSettingsException _ _ -> (Nothing, "loading settings exception")
  Yaml.NonStringKey _ -> (Nothing, "non string key")
  Yaml.MultipleDocuments -> (Nothing, "multiple documents")

succeeds :: Alternative m => m () -> m Bool
succeeds m = True <$ m <|> pure False

nonEmptyLine :: BParser Text
nonEmptyLine = takeWhile1P Nothing notNewline

sc :: MonadParsec e Text m => m ()
sc = void $ takeWhileP (Just "white space") isSpaceN

sc' :: MonadParsec e Text m => m ()
sc' = void $ takeWhileP (Just "white space") isSpace

eol :: MonadParsec e Text m => m ()
eol =
  void . label "newline" $
    choice
      [ string "\n"
      , string "\r\n"
      , string "\r"
      ]

eol' :: MonadParsec e Text m => m Bool
eol' = option False (True <$ eol)

isSpace :: Char -> Bool
isSpace x = x == ' ' || x == '\t'

isSpaceN :: Char -> Bool
isSpaceN x = isSpace x || isNewline x

isNewline :: Char -> Bool
isNewline x = x == '\n' || x == '\r'

notNewline :: Char -> Bool
notNewline = not . isNewline

pInlinesTop :: IParser (NonEmpty Inline)
pInlinesTop = undefined

prependErr :: Int -> ZtlMarkError -> [Block Isp] -> [Block Isp]
prependErr o custom blocks = Naked (IspError e) : blocks
  where
    e = FancyError o (E.singleton $ ErrorCustom custom)
ilevel :: Pos -> Pos
ilevel = (<> mkPos 4)

isBlank :: Text -> Bool
isBlank = T.all isSpace

assembleParagraph :: [Text] -> Text
assembleParagraph = go
  where
    go [] = ""
    go [x] = T.dropWhileEnd isSpace x
    go (x : xs) = x <> "\n" <> go xs

replaceEof :: forall e. Show e => String -> ParseError Text e -> ParseError Text e
replaceEof altLabel = \case
  TrivialError pos us es -> TrivialError pos (f <$> us) (E.map f es)
  FancyError pos xs -> FancyError pos xs
  where
    f EndOfInput = Label (NE.fromList altLabel)
    f x = x
