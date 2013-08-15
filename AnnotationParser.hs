module AnnotationParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Perm

import Control.Applicative ((<$>))

import AnnotationTypes
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

car :: Parsec String u Annotation
car = do
    symbol "Car"
    pos <- xy
    cl <- colour
    skipMany $ char ' '
    ang <- angle
    sz <- size
    return $ Car cl pos ang sz "foo" E 0 1

xy :: Parsec String u (Double, Double)
xy = do
    symbol "@"
    x <- floatOrInteger
    -- Separation with spaces is implied.
    y <- floatOrInteger
    return (x, y) -- Bounds?

angle :: Parsec String u Double
angle = do
    symbol "^"
    floatOrInteger

colour :: Parsec String u (Colour Double)
colour = do
    symbol "#"
    many1 alphaNum >>= readColourName

size :: Parsec String u Double
size = do
    symbol "%"
    floatOrInteger

caption :: Parsec String u (CaptionAlignment, Double, Double, String)
caption = do
    txt <- stringLiteral
    (al, sz, ang) <- braces $ do
        al' <- alignment
        sz' <- size
        ang' <- angle
        -- TODO: colour (?)
        return (al', sz', ang')
    return (al, sz, ang, txt)

alignment :: Parsec String u CaptionAlignment
alignment = do
    symbol "'"
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")

floatOrInteger = try float <|> (fromIntegral <$> integer)

lexer = P.makeTokenParser haskellDef

float = P.float lexer
integer = P.integer lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer
braces = P.braces lexer
parens = P.parens lexer


test = runP car () "Test" "Car @13 17.2 #red^ 45  %1 \"Friker\" { '  S ^90 %1}"

main = putStrLn $ show test
