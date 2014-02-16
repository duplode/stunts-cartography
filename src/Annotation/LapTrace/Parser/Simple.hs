{-# LANGUAGE NoMonomorphismRestriction #-}
module Annotation.LapTrace.Parser.Simple
    ( rawtrace
    , laptrace
    ) where

import Control.Monad
import Control.Applicative (pure, (<*>), (<*), (*>), (<$>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim

import Annotation.LapTrace.Vec

-- Parser for raw coordinates extracted from game data as provided by the
-- repldump2carto tool (cf. the REPLDUMP.md document and the repldump
-- directory). The format is very simple: each line is a frame; six integers
-- per line, with the first three being the player position vector and the
-- other ones the car orientation vector.

eol = oneOf "\n\r"

properSpace = many (oneOf "\t ")

colInteger = (<?> "an integer") $ fmap (read :: String -> Integer) $
    properSpace *>
    (try (fmap (:) $ char '-') <|> pure id) <*> many1 digit
    <* properSpace

vecWide =  pure (,,) <*> colInteger <*> colInteger <*> colInteger

frame = pure (,) <*> vecWide <*> vecWide <* eol

rawtrace = frame `manyTill` eof

laptrace = map processFrame <$> rawtrace
    where
    processFrame (xyz, rot) = (scaleRawCoords xyz, scaleRawRot rot)
