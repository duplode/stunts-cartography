{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Annotation.LapTrace.Parser.Simple
    ( rawtrace
    , laptrace
    ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim

import Annotation.LapTrace.Vec

-- Parser for raw coordinates extracted from game data as provided by the
-- repldump2carto tool (cf. the REPLDUMP.md document and the repldump
-- directory). The format is very simple:
--
-- * Each line is a frame;
--
-- * Six obligatory integers, followed by two optional ones, separated
--   by spaces or tabs.
--
--     * The first three integers are the player position vector.
--
--     * The next three are the player position vector.
--
--     * The optional integers are the player speed and gear.
--
-- The speed and gear fields are optional so that the parser accepts
-- repldump2carto output from version 0.4.
--
-- All values are assumed to be in the appopriate internal game units.

eol :: Stream s m Char => ParsecT s u m Char
eol = oneOf "\n\r"

properSpace :: Stream s m Char => ParsecT s u m String
properSpace = many (oneOf "\t ")

colInteger :: (Stream s m Char, Num a, Read a) => ParsecT s u m a
colInteger = (<?> "an integer") $ fmap read $
    properSpace *>
    (try (fmap (:) $ char '-') <|> pure id) <*> many1 digit
    <* properSpace

vecWide :: Stream s m Char => ParsecT s u m VecWide
vecWide = (,,) <$> colInteger <*> colInteger <*> colInteger

frame
    :: Stream s m Char
    => ParsecT s u m (VecWide, VecWide, Maybe Integer, Maybe Int)
frame = (,,,)
    <$> vecWide <*> vecWide
    <*> optionMaybe colInteger <*> optionMaybe colInteger
    <* eol

rawtrace
    :: Stream s m Char
    => ParsecT s u m [(VecWide, VecWide, Maybe Integer, Maybe Int)]
rawtrace = frame `manyTill` eof

laptrace :: Stream s m Char => ParsecT s u m [PreTracePoint]
laptrace = map processFrame <$> rawtrace
    where
    processFrame (xyz, rot, spd, gear) = PreTracePoint
        { preTracePos = scaleRawCoords xyz
        , preTraceRot = scaleRawRot rot
        , preTraceSpeed = scaleRawSpeed <$> spd
        , preTraceGear = gear
        }
