{-# LANGUAGE FlexibleContexts #-}
module Text.Parser.Braille (Braille(..), BrailleParsing(..), digit, number) where

import           Control.Applicative                        (some)
import           Control.Monad                              (MonadPlus())
import           Control.Monad.Trans.Class                  (lift)
import           Control.Monad.Trans.Identity               (IdentityT())
import           Control.Monad.Trans.Reader                 (ReaderT())
import qualified Control.Monad.Trans.State.Lazy   as Lazy   (StateT())
import qualified Control.Monad.Trans.State.Strict as Strict (StateT())
import           Data.Bits                                  (bit, (.|.))
import           Data.Functor                               (($>))
import           Data.Foldable                              (asum, toList)
import qualified Text.Parsec as Parsec                      (Stream, ParsecT())
import           Text.Parser.Char                           (CharParsing(satisfy))
import           Text.Parser.Combinators                    ((<?>))
import qualified Text.Trifecta as Trifecta                  (Parser)

class Enum b => Braille b where
  toChar :: b -> Char
  toChar = toEnum . (+ 0x2800) . fromEnum

instance Braille Char where toChar = id

instance Braille Int where
  toChar = toEnum . (+ 0x2800) . fromDecimal where
    fromDecimal 0 = 0
    fromDecimal b = ((.|.) <$> bit.pred.snd <*> fromDecimal.fst) $ b `divMod` 10

class CharParsing m => BrailleParsing m where
  brl :: Braille b => b -> m b
  brl b = satisfy (== toChar b) $> b <?> [toChar b]

  cells :: (Traversable t, Braille b) => t b -> m (t b)
  cells bs = traverse brl bs <?> toList (fmap toChar bs)

instance (BrailleParsing m, MonadPlus m) => BrailleParsing (IdentityT m) where
  brl = lift . brl
  cells = lift . cells

instance (BrailleParsing m, MonadPlus m) => BrailleParsing (ReaderT e m) where
  brl = lift . brl
  cells = lift . cells

instance (BrailleParsing m, MonadPlus m) => BrailleParsing (Lazy.StateT s m) where
  brl = lift . brl
  cells = lift . cells

instance (BrailleParsing m, MonadPlus m) => BrailleParsing (Strict.StateT s m) where
  brl = lift . brl
  cells = lift . cells

instance Parsec.Stream s m Char => BrailleParsing (Parsec.ParsecT s u m)
instance BrailleParsing Trifecta.Parser

digit :: (BrailleParsing m, Enum a, Num a) => m a
digit = asum $ zipWith ($>) (map brl "⠚⠁⠃⠉⠙⠑⠋⠛⠓⠊") [0..]

number :: (BrailleParsing m, Enum a, Num a) => m a
number = brl '⠼' *> (foldl ((+) . (10 *)) 0 <$> some digit)
