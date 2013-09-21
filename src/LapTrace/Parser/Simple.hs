{-# LANGUAGE NoMonomorphismRestriction #-}
module LapTrace.Parser.Simple
    ( laptrace
    ) where

import Control.Monad
import Control.Applicative (pure, (<*>), (<*), (*>), (<$>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim

-- Parser for raw coordinates extracted from game data. The format is very
-- simple: each line is a frame; six integers per line, with the first
-- three being the player position vector and the other ones the car
-- orientation vector.

type VecWide = (Integer, Integer, Integer)

eol = oneOf "\n\r"

properSpace = many (oneOf "\t ")

colInteger = (<?> "an integer") $ fmap (read :: String -> Integer) $
    properSpace *>
    (try (fmap (:) $ char '-') <|> pure id) <*> many1 digit
    <* properSpace

vecWide =  pure (,,) <*> colInteger <*> colInteger <*> colInteger

frame = pure (,) <*> vecWide <*> vecWide <* eol

laptrace = frame `manyTill` eof
