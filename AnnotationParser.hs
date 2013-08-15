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
    opt <- permute ((,,,) <$?> (yellow, colour)
                          <|?> (0, skipMany (char ' ') >> angle)
                          <|?> (0.5, size)
                          <|?> ((E, 0.5, 0, yellow, ""), caption))
    let (cl, ang, sz, capt) = opt
    let (cpAl, cpSz, cpAng, _, cpTxt) = capt
    return $ Car cl pos ang sz cpTxt cpAl cpAng cpSz

xy :: Parsec String u (Double, Double)
xy = do
    symbol "@"
    x <- floatOrInteger
    -- Separation with spaces is implied.
    y <- floatOrInteger
    return (x, y) -- Bounds?

xyInt :: Parsec String u (Int, Int)
xyInt = do
    symbol "@"
    x <- fromIntegral <$> integer
    y <- fromIntegral <$> integer
    return (x, y)

angle :: Parsec String u Double
angle = do
    symbol "^"
    floatOrInteger

colour :: Parsec String u (Colour Double)
colour = do
    symbol "#"
    -- TODO: add triplet support (thankfully readColourName fails with fail).
    many1 alphaNum >>= readColourName

size :: Parsec String u Double
size = do
    symbol "%"
    floatOrInteger

caption :: Parsec String u
               (CaptionAlignment, Double, Double, (Colour Double), String)
caption = do
    txt <- stringLiteral
    (al, sz, ang, cl) <- option (E, 0.5, 0, yellow) . try . braces $
        permute ((,,,) <$?> (E, alignment)
                       <|?> (0.5, size)
                       <|?> (0, angle)
                       <|?> (yellow, colour))
    return (al, sz, ang, cl, txt)

alignment :: Parsec String u CaptionAlignment
alignment = do
    symbol "'"
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")

floatOrInteger = try float <|> fromIntegral <$> integer

lexer = P.makeTokenParser haskellDef

float = P.float lexer
integer = P.integer lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer
braces = P.braces lexer
parens = P.parens lexer


test = runP car () "Test" "Car @13 17.2 #red^ 45  %1 \"Friker\" { '  S %1 ^ 90}"
test2 = runP car () "Test" "Car @13 17.2"

main = putStrLn $ show test
