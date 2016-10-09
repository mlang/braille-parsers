module Main where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.ByteString.UTF8 as UTF8
import qualified Text.Trifecta as Trifecta
import qualified Text.Trifecta.Delta as Trifecta
import Text.Parser.Combinators
import Text.Parser.Braille

type Octave = Int
type Parser = StateT (Maybe Octave) Trifecta.Parser

run :: Parser a -> Maybe Octave -> String -> Trifecta.Result a
run p i = Trifecta.parseString (evalStateT p i) (name "") where
  name s = Trifecta.Directed (UTF8.fromString s) 0 0 0 0

x :: (BrailleParsing m, MonadPlus m) => StateT (Maybe Octave) m ()
x = do
  put $ Just (1 :: Int)
  _ <- brl (12 :: Int)
  pure ()

y :: Parser Octave
y = do
  x <- get
  case x of
    Just x' -> pure x'
    _       -> unexpected "lalala"

main :: IO ()
main = do
  case (run number Nothing "⠼⠁⠃⠉⠙⠑⠋⠛") of
    Trifecta.Success a -> putStrLn $ show a
    _                  -> putStrLn "No result"

