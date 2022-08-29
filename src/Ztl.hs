module Ztl where

import Protolude

import Polysemy

data Teletype m a where
  ReadTTY :: Teletype m Text
  WriteTTY :: Text -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _ -> writeTTY i >> echo

-- echo forever
main :: IO ()
main = runM . teletypeToIO $ echo
