module Ztl where

import Protolude

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m Text
  WriteTTY :: Text -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletypePure :: [Text] -> Sem (Teletype ': r) a -> Sem r ([Text], a)
runTeletypePure i
  -- For each WriteTTY in our program, consume an output by appending it to the
  -- list in a ([String], a)
  = runOutputMonoid pure
  -- Treat each element of our list of strings as a line of input
  . runInputList i
  -- Reinterpret our effect in terms of Input and Output
  . reinterpret2 \case
      ReadTTY -> maybe "" identity <$> input
      WriteTTY msg -> output msg


echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo


-- Let's pretend
echoPure :: [Text] -> Sem '[] ([Text], ())
echoPure = flip runTeletypePure echo

pureOutput :: [Text] -> [Text]
pureOutput = fst . run . echoPure

-- echo forever
main :: IO ()
main = runM . teletypeToIO $ echo