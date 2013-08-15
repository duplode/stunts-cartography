module AnnotationParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Perm

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)

import AnnotationTypes
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB

car :: Parsec String u Annotation
car = do
    symbol "Car"
    opt <- permute ((,,,,) <$$> xy
                           <|?> (yellow, colour)
                           <|?> (0, skipMany (char ' ') >> angle)
                           <|?> (0.5, size)
                           <|?> ((E, 0.5, 0, Nothing, ""), caption))
    let (pos, cl, ang, sz, capt) = opt
    let (cpAl, cpSz, cpAng, _, cpTxt) = capt -- TODO: don't ignore the colour.
    return $ Car cl pos ang sz cpTxt cpAl cpAng cpSz

seg :: Parsec String u Annotation
seg = do
    symbol "Seg"
    opt <- permute ((,,,,) <$$> xy
                           <|?> (yellow, colour)
                           <||> (skipMany (char ' ') >> angle)
                           <||> size
                           <|?> ((E, 0.5, 0, Nothing, ""), caption))
    let (pos, cl, ang, len, capt) = opt
    let (cpAl, cpSz, cpAng, _, cpTxt) = capt -- TODO: don't ignore the colour.
    return $ Seg cl pos ang len cpTxt cpAl cpAng cpSz

splitSeg :: Parsec String u Annotation
splitSeg = do
    symbol "Split"
    ix <- fromIntegral <$> integer
    opt <- permute ((,,,,) <$$> xyInt
                           <|?> (yellow, colour)
                           <||> splitDir
                           <||> sizeInt
                           <|?> (Nothing, Just <$> alignment))
    let (pos, cl, splD, len, mCaptAl) = opt
    let captAl = fromMaybe (if splD == H then E else N) mCaptAl
    return $ Split cl ix pos splD len captAl

xy :: Parsec String u (Double, Double)
xy = do
    symbol "@"
    x <- floatOrInteger
    -- Separation with spaces is implied.
    y <- floatOrInteger
    return (x, y) -- TODO: bounds?

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

sizeInt :: Parsec String u Int
sizeInt = do
    symbol "%"
    fromIntegral <$> integer

-- On the Maybe (Colour Double): Nothing means "use a default from somewhere".
caption :: Parsec String u
               ( CaptionAlignment, Double, Double
               , Maybe (Colour Double), String )
caption = do
    txt <- stringLiteral
    (al, sz, ang, mCl) <- option (E, 0.5, 0, Nothing) . try . braces $
        permute ((,,,) <$?> (E, alignment)
                       <|?> (0.5, size)
                       <|?> (0, angle)
                       <|?> (Nothing, Just <$> colour))
    return (al, sz, ang, mCl, txt)

alignment :: Parsec String u CaptionAlignment
alignment = do
    symbol "'"
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")

splitDir :: Parsec String u SplitDirection
splitDir = do
    symbol "!"
    read <$> (
        try (symbol "H")
        <|> symbol "V")

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
