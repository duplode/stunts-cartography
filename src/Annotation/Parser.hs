{-# LANGUAGE NoMonomorphismRestriction #-}
module Annotation.Parser
    ( parseAnnotations
    , annotations
    , pAnnotation
    ) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Permutation

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*), (<*>))
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class
import System.Directory (doesFileExist)

import Annotation
import Annotation.LapTrace
import Annotation.LapTrace.Parser.Simple
import Data.Colour (Colour)
import Data.Colour.Names (readColourName, yellow)
import Data.Colour.SRGB (sRGB24read)
import Types.CartoM
import Control.Monad.RWS (tell)
import qualified Parameters as Pm

parseAnnotations :: (MonadIO m) => String -> CartoT m [Annotation]
parseAnnotations input = do
    result <- runPT annotations () "" input
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
    <|> try (annotation <$> splitSeg)
    <|> try (annotation <$> traceSpec)) <* annDelimiter

annDelimiter = ((detectAnnStart <|> try semi) >> return ()) <|> eof

detectAnnStart = choice . map (lookAhead . try . symbol) $
    ["Car", "Seg", "Split", "Trace"]

car = do
    symbol "Car"
    opt <- runPermParser $
        (,,,,) <$> oncePerm xy
               <*> optionPerm yellow colour
               <*> optionPerm 0 angle
               <*> optionPerm 0.5 size
               <*> optionPerm (Nothing, 0, 0, E, 0.4, "") caption
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
    opt <- runPermParser $
        (,,,,) <$> oncePerm xy
               <*> optionPerm yellow colour
               <*> oncePerm angle
               <*> oncePerm size
               <*> optionPerm (Nothing, 0, 0, E, 0.4, "") caption
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
    opt <- runPermParser $
        (,,,,,) <$> oncePerm xyInt
                <*> optionPerm yellow colour
                <*> optionPerm 0 bg
                <*> oncePerm splitDir
                <*> oncePerm sizeInt
                <*> optionMaybePerm alignment
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
caption :: (Monad m) => ParsecT String u m
               ( Maybe (Colour Double), Double, Double, CardinalDirection
               , Double, String )
caption = do
    txt <- stringLiteral
    (al, sz, ang, mCl, bg) <- option (E, 0.4, 0, Nothing, 0) . try . braces $
        runPermParser $ (,,,,) <$> optionPerm E alignment
                               <*> optionPerm 0.4 size
                               <*> optionPerm 0 angle
                               <*> optionMaybePerm colour
                               <*> optionPerm 0 bg
    return (mCl, bg, ang, al, sz, txt)

cardinalDir :: (Monad m) => String -> ParsecT String u m CardinalDirection
cardinalDir leading = do
    symbol leading
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")

alignment = cardinalDir "'"

splitDir = cardinalDir "^"


traceSpec = do
    symbol "Trace"
    opt <- runPermParser $
        (,,,) <$> oncePerm rawPath
              <*> optionPerm yellow colour
              <*> optionPerm True visibility
              <*> manyPerm carOnTrace
    let (path, cl, vis, cars) = opt
    -- TODO: Add support for relative paths via CartoT
    exists <- liftIO $ doesFileExist path
    if not exists then
        fail "Trace data file does not exist."
    else do
        rawData <- liftIO $ readFile path
        let eDat = runP laptrace () path rawData
        case eDat of
            Left e    -> fail $ show e
            Right dat -> return $ initializeTrace dat emptyTraceAnn
                { traceAnnPoints = []
                , traceAnnOverlays = emptyTraceOverlays
                    { carsOverTrace = map
                        (fmap $ overrideCarAnnColours cl) cars
                    }
                , traceAnnColour = cl
                , traceAnnVisible = vis
                }

-- As the name indicates, the "path" could be anything.
rawPath = do
    symbol ":"
    stringLiteral

visibility = do
    symbol "!"
    return False

lapMoment = do
    symbol "@"
    floatOrInteger

-- TODO: Minimize duplication in the car parsers. First step would be
-- simplifying the caption parser interface.
-- TODO: Add support for overriding the colours (trickier than it sounds).
carOnTrace = (symbol "+" >>) $ braces $ do
    symbol "Car"
    opt <- runPermParser $
        (,,) <$> oncePerm lapMoment
             <*> optionPerm 0.5 size
             <*> optionPerm (Nothing, 0, 0, E, 0.4, "") caption
    let (moment, sz, capt) = opt
    -- TODO: Stop ignoring the caption colour.
    let (_, cpBg, cpAng, cpAl, cpSz, cpTxt) = capt
    return $ (truncate $ 20 * moment, CarAnnotation
        { carAnnColour = yellow
        , carAnnPosition = (0, 0)
        , carAnnAngle = 0
        , carAnnSize = sz
        , carAnnCaption = CaptAnnotation
            { captAnnPosition = (0, 0)
            , captAnnText = cpTxt
            , captAnnColour = yellow
            , captAnnBgOpacity = cpBg
            , captAnnAlignment = cpAl
            , captAnnAngle = cpAng
            , captAnnSize = cpSz
            }
        })


floatOrInteger = try float <|> fromIntegral <$> integer

lexer = P.makeTokenParser annLang

-- Mostly copied from the doc; doesn't really matter for now.
annLang = P.LanguageDef
    { P.commentStart = "" -- TODO: Might be useful.
    , P.commentEnd = ""
    , P.commentLine = ""
    , P.nestedComments = False
    , P.identStart = letter <|> char '_'
    , P.identLetter = alphaNum <|> char '_'
    , P.opStart = P.opLetter annLang
    , P.opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , P.reservedOpNames = []
    , P.reservedNames = []
    , P.caseSensitive = True
    }

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

runTests = mapM parseAnnotations $
    [test1, test2, test3, test4, test5, test6, test7]

