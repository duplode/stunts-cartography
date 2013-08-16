module AnnotationParser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Perm

import Control.Applicative ((<$>), (<*))
import Data.Maybe (fromMaybe)

import AnnotationTypes
import Data.Colour
import Data.Colour.Names
import Data.Colour.SRGB


parseAnnotations :: String -> [Annotation]
parseAnnotations input = either (const []) id $ runP annotations () "" input

-- TODO: Add useful error messages.
-- TODO: Collect errors (possibly by injecting a Writer).
annotations :: Parsec String u [Annotation]
annotations = whiteSpace >> annotation `manyTill` eof

annotation :: Parsec String u Annotation
annotation = (try car <|> try seg <|> splitSeg) <* annDelimiter

annDelimiter :: Parsec String u ()
annDelimiter = ((detectAnnStart <|> try semi) >> return ()) <|> eof

detectAnnStart :: Parsec String u String
detectAnnStart = choice . map (lookAhead . try . symbol) $ ["Car", "Seg", "Split"]

car :: Parsec String u Annotation
car = do
    symbol "Car"
    opt <- permute ((,,,,) <$$> xy
                           <|?> (yellow, colour)
                           <|?> (0, angle)
                           <|?> (0.5, size)
                           <|?> ((Nothing, 0, E, 0.5, ""), caption))
    let (pos, cl, ang, sz, capt) = opt
    let (_, cpAng, cpAl, cpSz, cpTxt) = capt -- TODO: don't ignore the colour.
    return $ Car cl pos ang sz cpTxt cpAl cpAng cpSz

seg :: Parsec String u Annotation
seg = do
    symbol "Seg"
    opt <- permute ((,,,,) <$$> xy
                           <|?> (yellow, colour)
                           <||> angle
                           <||> size
                           <|?> ((Nothing, 0, E, 0.5, ""), caption))
    let (pos, cl, ang, len, capt) = opt
    let (_, cpAng, cpAl, cpSz, cpTxt) = capt -- TODO: don't ignore the colour.
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
    let captAl = fromMaybe splD mCaptAl
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
    many1 alphaNum >>= ((skipMany (char ' ') >>) . readColourName)

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
               ( Maybe (Colour Double), Double, CardinalDirection
               , Double, String )
caption = do
    txt <- stringLiteral
    (al, sz, ang, mCl) <- option (E, 0.5, 0, Nothing) . try . braces $
        permute ((,,,) <$?> (E, alignment)
                       <|?> (0.5, size)
                       <|?> (0, angle)
                       <|?> (Nothing, Just <$> colour))
    return (mCl, ang, al, sz, txt)

cardinalDir :: String -> Parsec String u CardinalDirection
cardinalDir leading = do
    symbol leading
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")


alignment :: Parsec String u CardinalDirection
alignment = cardinalDir "'"

splitDir :: Parsec String u CardinalDirection
splitDir = cardinalDir "^"

floatOrInteger = try float <|> fromIntegral <$> integer

lexer = P.makeTokenParser haskellDef

float = P.float lexer
integer = P.integer lexer
stringLiteral = P.stringLiteral lexer
symbol = P.symbol lexer
braces = P.braces lexer
parens = P.parens lexer
semi = P.semi lexer
whiteSpace = P.whiteSpace lexer

test1 = "Car @13 17.2 ^ 45 %1#red \"Friker\" { %1 ^ 90 '  S }"
test2 = "Car @13 17.2; "
test3 = "Car &13 17.2;"
test4 = "Car @13 17.2 ^ 45 %1 #slateblue \"Friker\" {  %1 ^ 90'  S} ; "
test5 = "Blub"
test6 = "Car @15.5 10.5 ^135 %0.5 #yellow \"foo\" {^0 %1 'N} ;\n\n"
    ++ "Car @16.5 10.5 ^160 %0.5 #magenta \"bar\" {^0 %1 'E};\n"
    ++ "Split 1 @22 11 ^N %5 #magenta 'N;\n"
    ++ "Split 1 @22 \n    11 ^N %5 #green 'N;\n"
    ++ "Split 1 @22 11 ^N %5 #aliceblue 'N"
    ++ "Split 1 %5 #white @22 11 ^N 'N;\n"
    ++ "Seg @16.5 10.5 ^160 %0.5 #magenta \"bar\" {^0 %1 'E};\n"
    ++ "Seg @16.5 10.5 ^160 %0.5 #red \"bar\" {^0 %1 'E}"
test7 = "Split 1 @22 11 ^N %5 #magenta 'N;"

runTests = map parseAnnotations $
    [test1, test2, test3, test4, test5, test6, test7]

eol :: Parsec String u String
eol =    try (string "\n\r")
     <|> try (string "\r\n")
     <|> string "\n"
     <|> string "\r"

