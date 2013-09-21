module Annotation.Parser
    ( parseAnnotations
    , annotations
    , pAnnotation
    ) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Text.Parsec.Perm

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*))
import Data.Maybe (fromMaybe)

import Annotation
import Data.Colour (Colour)
import Data.Colour.Names (readColourName, yellow)
import Data.Colour.SRGB (sRGB24read)
import Types.CartoM
import Control.Monad.RWS (tell)
import qualified Parameters as Pm

-- Note that the combinators in Text.Parsec.Perm have types built around
-- Parsec and not ParsecT.
parseAnnotations :: (Monad m) => String -> CartoT m [Annotation]
parseAnnotations input = do
    let result = runP annotations () "" input
    case result of
        Left err   -> do
            tell . Pm.logFromList $
                "Error in defining the annotations "
            tell . Pm.logFromList . show $ err
            tell . Pm.logFromList $ "\r\n"
            return []
        Right anns -> return anns

-- TODO: Add better error messages.
annotations = whiteSpace >> pAnnotation `manyTill` eof

pAnnotation = (try (annotation <$> car)
    <|> try (annotation <$> seg)
    <|> try (annotation <$> splitSeg)) <* annDelimiter

annDelimiter = ((detectAnnStart <|> try semi) >> return ()) <|> eof

detectAnnStart = choice . map (lookAhead . try . symbol) $ ["Car", "Seg", "Split"]

car = do
    symbol "Car"
    opt <- permute ((,,,,) <$$> xy
                           <|?> (yellow, colour)
                           <|?> (0, angle)
                           <|?> (0.5, size)
                           <|?> ((Nothing, 0, 0, E, 0.4, ""), caption))
    let (pos, cl, ang, sz, capt) = opt
    let (mCpCl, cpBg, cpAng, cpAl, cpSz, cpTxt) = capt
    return $ CarAnnotation
        { carAnnColour = cl
        , carAnnPosition = pos
        , carAnnAngle = ang
        , carAnnSize = sz
        , carAnnCaption = CaptAnnotation
            { captAnnPosition = (0, 0) -- Doesn't matter.
            , captAnnText = cpTxt
            , captAnnColour = fromMaybe cl mCpCl
            , captAnnBgOpacity = cpBg
            , captAnnAlignment = cpAl
            , captAnnAngle = cpAng
            , captAnnSize = cpSz
            }
        }

seg = do
    symbol "Seg"
    opt <- permute ((,,,,) <$$> xy
                           <|?> (yellow, colour)
                           <||> angle
                           <||> size
                           <|?> ((Nothing, 0, 0, E, 0.4, ""), caption))
    let (pos, cl, ang, len, capt) = opt
    let (mCpCl, cpBg, cpAng, cpAl, cpSz, cpTxt) = capt
    return $ SegAnnotation
        { segAnnColour = cl
        , segAnnPosition = pos
        , segAnnAngle = ang
        , segAnnLength = len
        , segAnnCaption = CaptAnnotation
            { captAnnPosition = (0, 0) -- Doesn't matter.
            , captAnnText = cpTxt
            , captAnnColour = fromMaybe cl mCpCl
            , captAnnBgOpacity = cpBg
            , captAnnAlignment = cpAl
            , captAnnAngle = cpAng
            , captAnnSize = cpSz
            }
        }

splitSeg = do
    symbol "Split"
    ix <- fromIntegral <$> integer
    opt <- permute ((,,,,,) <$$> xyInt
                            <|?> (yellow, colour)
                            <|?> (0, bg)
                            <||> splitDir
                            <||> sizeInt
                            <|?> (Nothing, Just <$> alignment))
    let (pos, cl, captBg, splD, len, mCaptAl) = opt
    let captAl = fromMaybe splD mCaptAl
    return $ SplitAnnotation
        { splAnnColour = cl
        , splAnnIndex = ix
        , splAnnPosition = pos
        , splAnnDirection = splD
        , splAnnLength = len
        , splAnnCaptBgOpacity = captBg
        , splAnnCaptAlignment = fromMaybe splD mCaptAl
        }

xy = do
    symbol "@"
    x <- floatOrInteger
    -- Separation with spaces is implied.
    y <- floatOrInteger
    return (x, y) -- TODO: bounds?

xyInt = do
    symbol "@"
    x <- fromIntegral <$> integer
    y <- fromIntegral <$> integer
    return (x, y)

angle = do
    symbol "^"
    floatOrInteger

-- TODO: add triplet support (see Data.Colour.SRGB)
-- It is convenient that readColourName fails with fail.
colour = do
    symbol "#"
    try (replicateM 6 hexDigit >>= ((skipMany space >>) . return . sRGB24read))
        <|> (many1 alphaNum >>= ((skipMany space >>) . readColourName))

-- For the moment, this only parses the background opacity.
bg = do
    symbol "$"
    (/100) <$> floatOrInteger

size = do
    symbol "*"
    floatOrInteger

sizeInt = do
    symbol "*"
    fromIntegral <$> integer

-- Captions associated to another annotation.
-- On the Maybe (Colour Double): Nothing means "use a default from somewhere".
-- TODO: Stop returning a 6-uple, ideally as soon as a clean way of handling
-- the optional colour is found.
caption :: Parsec String u
               ( Maybe (Colour Double), Double, Double, CardinalDirection
               , Double, String )
caption = do
    txt <- stringLiteral
    (al, sz, ang, mCl, bg) <- option (E, 0.4, 0, Nothing, 0) . try . braces $
        permute ((,,,,) <$?> (E, alignment)
                        <|?> (0.4, size)
                        <|?> (0, angle)
                        <|?> (Nothing, Just <$> colour)
                        <|?> (0, bg))
    return (mCl, bg, ang, al, sz, txt)

cardinalDir :: String -> Parsec String u CardinalDirection
cardinalDir leading = do
    symbol leading
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")


alignment = cardinalDir "'"

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

test1 = "Car @13 17.2 ^ 45 *1#red \"Friker\" { *1 ^ 90 '  S }"
test2 = "Car @13 17.2; "
test3 = "Car &13 17.2;"
test4 = "Car @13 17.2 ^ 45 *1 #slateblue \"Friker\" {  *1 ^ 90'  S} ; "
test5 = "Blub"
test6 = "Car @15.5 10.5 ^135 *0.5 #yellow \"foo\" {^0 *1 'N} ;\n\n"
    ++ "Car @16.5 10.5 ^160 *0.5 #magenta \"bar\" {^0 *1 'E};\n"
    ++ "Split 1 @22 11 ^N *5 #magenta 'N;\n"
    ++ "Split 1 @22 \n    11 ^N *5 #green 'N;\n"
    ++ "Split 1 @22 11 ^N *5 #aliceblue 'N"
    ++ "Split 1 *5 #white @22 11 ^N 'N;\n"
    ++ "Seg @16.5 10.5 ^160 *0.5 #magenta \"bar\" {^0 *1 'E};\n"
    ++ "Seg @16.5 10.5 ^160 *0.5 #red \"bar\" {^0 *1 'E}"
test7 = "Split 1 @22 11 ^N *5 #magenta 'N;"

runTests :: CartoM [[Annotation]]
runTests = mapM parseAnnotations $
    [test1, test2, test3, test4, test5, test6, test7]

