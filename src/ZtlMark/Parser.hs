module ZtlMark.Parser (
  parse,
  ZtlMarkError (..),
) where

import Control.Applicative (Alternative, liftA2)
import Control.Monad
import Data.Bifunctor (Bifunctor (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Text (Text)
import Text.Megaparsec hiding (State (..), parse)
import Text.Megaparsec.Char hiding (eol)
import ZtlMark.Parser.Internal
import ZtlMark.Type

import Data.Aeson qualified as Aeson
import Data.Char qualified as Char
import Data.DList qualified as DList
import Data.List.NonEmpty qualified as NE
import Data.Maybe (catMaybes)
import Data.Set qualified as E
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Yaml qualified as Yaml
import Text.Megaparsec.Char.Lexer qualified as L

-- | Frame that describes where we are in parsing inlines.
data InlineFrame
  = -- | Strong emphasis with asterisk @**@
    StrongFrame
  | -- | Strong emphasis with underscore @__@
    StrongFrame_
  deriving stock (Eq, Ord, Show)

{- | State of inline parsing that specifiec wheater we expect to close on
 frame or there is a possibility to close ano of two alternatives.
-}
data InlineState
  = -- | One frame to be closed
    SingleFrame InlineFrame
  | -- | Two frames to be closed
    DoubleFrame InlineFrame InlineFrame
  deriving stock (Eq, Ord, Show)

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
pInlinesTop = do
  inlines <- pInlines
  eof <|> void pLfdr
  pure inlines

pInlines :: IParser (NonEmpty Inline)
pInlines = do
  done <- atEnd
  allowsEmpty <- isEmptyAllowed
  if done
    then
      if allowsEmpty
        then (pure . nes . Plain) ""
        else unexpEic EndOfInput
    else NE.some1 $ do
      mch <- lookAhead (anySingle <?> "inline content")
      case mch of
        ch ->
          if isFrameConstituent ch
            then pEnclosedInline
            else pPlain

pPlain :: IParser Inline
pPlain = fmap (Plain . bakeText) . foldSome $ do
  ch <- lookAhead (anySingle <?> "inline content")
  let newline' =
        (('\n' :) . dropWhile isSpace) <$ eol <* sc' <* lastChar SpaceChar
  case ch of
    '\\' ->
      (:)
        <$> ( (escapedChar <* lastChar OtherChar)
                <|> try (char '\\' <* notFollowedBy eol <* lastChar OtherChar)
            )
    '\n' -> newline'
    '\r' -> newline'
    _ ->
      (:)
        <$> if Char.isSpace ch
          then char ch <* lastChar SpaceChar
          else
            if isSpecialChar ch
              then
                failure
                  (Just . Tokens . nes $ ch)
                  (E.singleton . Label . NE.fromList $ "inline content")
              else
                if Char.isPunctuation ch
                  then char ch <* lastChar PunctChar
                  else char ch <* lastChar OtherChar

pEnclosedInline :: IParser Inline
pEnclosedInline =
  disallowEmpty $
    pLfdr >>= \case
      SingleFrame x ->
        liftFrame x <$> pInlines <* pRfdr x
      DoubleFrame x y -> do
        inlines0 <- pInlines
        thisFrame <- pRfdr x <|> pRfdr y
        let thatFrame = if thisFrame == x then y else x
        minlines1 <- optional pInlines
        void (pRfdr thatFrame)
        pure . liftFrame thatFrame $
          case minlines1 of
            Nothing ->
              nes (liftFrame thisFrame inlines0)
            Just inlines1 ->
              liftFrame thisFrame inlines0 <| inlines1

pLfdr :: IParser InlineState
pLfdr = try $ do
  o <- getOffset
  let r st = st <$ string (inlineStateDel st)
  st <-
    hidden $
      choice
        [ r (DoubleFrame StrongFrame StrongFrame)
        , r (SingleFrame StrongFrame)
        , r (DoubleFrame StrongFrame_ StrongFrame_)
        , r (SingleFrame StrongFrame_)
        ]
  let dels = inlineStateDel st
      failNow =
        customFailure' o (NonFlankingDelimiterRun (toNesTokens dels))
  lch <- getLastChar
  rch <- getNextChar OtherChar
  when (lch >= rch) failNow
  return st

pRfdr :: InlineFrame -> IParser InlineFrame
pRfdr frame = try $ do
  let dels = inlineFrameDel frame
      expectingInlineContent = region $ \case
        TrivialError pos us es ->
          TrivialError pos us $
            E.insert (Label $ NE.fromList "inline content") es
        other -> other
  o <- getOffset
  (void . expectingInlineContent . string) dels
  let failNow =
        customFailure' o (NonFlankingDelimiterRun (toNesTokens dels))
  lch <- getLastChar
  rch <- getNextChar SpaceChar
  when (lch <= rch) failNow
  return frame

escapedChar :: MonadParsec e Text m => m Char
escapedChar =
  label "escaped character" $
    try (char '\\' *> satisfy isAsciiPunctuation)

isMarkupChar :: Char -> Bool
isMarkupChar x = isFrameConstituent x || f x
  where
    f = \case
      '[' -> True
      ']' -> True
      '`' -> True
      _ -> False

isSpecialChar :: Char -> Bool
isSpecialChar x = isMarkupChar x || x == '\\' || x == '!' || x == '<'

isAsciiPunctuation :: Char -> Bool
isAsciiPunctuation x =
  (x >= '!' && x <= '/')
    || (x >= ':' && x <= '@')
    || (x >= '[' && x <= '`')
    || (x >= '{' && x <= '~')

inlineStateDel :: InlineState -> Text
inlineStateDel = \case
  SingleFrame x -> inlineFrameDel x
  DoubleFrame x y -> inlineFrameDel x <> inlineFrameDel y

liftFrame :: InlineFrame -> NonEmpty Inline -> Inline
liftFrame = \case
  StrongFrame -> Strong
  StrongFrame_ -> Strong

inlineFrameDel :: InlineFrame -> Text
inlineFrameDel = \case
  StrongFrame -> "*"
  StrongFrame_ -> "_"

prependErr :: Int -> ZtlMarkError -> [Block Isp] -> [Block Isp]
prependErr o custom blocks = Naked (IspError e) : blocks
  where
    e = FancyError o (E.singleton $ ErrorCustom custom)
ilevel :: Pos -> Pos
ilevel = (<> mkPos 4)

isBlank :: Text -> Bool
isBlank = T.all isSpace

foldMany :: MonadPlus m => m (a -> a) -> m (a -> a)
foldMany f = go id
  where
    go g =
      optional f >>= \case
        Nothing -> pure g
        Just h -> go (h . g)

foldSome :: MonadPlus m => m (a -> a) -> m (a -> a)
foldSome f = liftA2 (flip (.)) f (foldMany f)

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

getNextChar :: CharType -> IParser CharType
getNextChar frameType = lookAhead (option SpaceChar (charType <$> anySingle))
  where
    charType ch
      | isFrameConstituent ch = frameType
      | Char.isSpace ch = SpaceChar
      | ch == '\\' = OtherChar
      | Char.isPunctuation ch = PunctChar
      | otherwise = OtherChar

isFrameConstituent :: Char -> Bool
isFrameConstituent = \case
  '*' -> True
  '^' -> True
  '_' -> True
  '~' -> True
  _ -> False

toNesTokens :: Text -> NonEmpty Char
toNesTokens = NE.fromList . T.unpack

unexpEic :: MonadParsec e Text m => ErrorItem Char -> m a
unexpEic x =
  failure
    (Just x)
    (E.singleton . Label . NE.fromList $ "inline content")

nes :: a -> NonEmpty a
nes a = a :| []

bakeText :: (String -> String) -> Text
bakeText = T.pack . reverse . ($ [])

customFailure' ::
  MonadParsec ZtlMarkError Text m =>
  Int ->
  ZtlMarkError ->
  m a
customFailure' o e =
  parseError $ FancyError o (E.singleton (ErrorCustom e))
