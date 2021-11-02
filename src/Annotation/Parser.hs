{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Annotation.Parser
    ( parseAnnotations
    , annotations
    , pAnnotation
    , parseFlipbook
    ) where

import Control.Lens.Operators
import qualified Control.Lens as L

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Permutation

import Control.Monad (replicateM, void)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Default.Class

import Annotation
import Annotation.LapTrace
import Annotation.LapTrace.Parser.Simple
import Annotation.Flipbook
import Data.Colour (Colour)
import Data.Colour.Names (readColourName)
import Data.Colour.SRGB (sRGB24read)
import Control.Monad.Writer.Class (MonadWriter, tell)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Parameters as Pm

-- The constraints on m can be thought of as "CartoT, but it shouldn't
-- touch the rendering state".
parseAnnotations
    :: (MonadIO m, MonadWriter Pm.RenderingLog m, MonadReader Pm.RenderingParameters m)
    => String -> m [Annotation]
parseAnnotations input = do
    result <- runPT annotations () "" input
    case result of
        Left err   -> do
            tell . Pm.logFromString $
                "Error in defining the annotations "
            tell . Pm.logFromString . show $ err
            tell . Pm.logFromString $ "\r\n\r\n"
            return []
        Right anns -> return anns

-- TODO: Add better error messages.
annotations
    :: (MonadIO m, MonadWriter Pm.RenderingLog m, MonadReader Pm.RenderingParameters m)
    => ParsecT String u m [Annotation]
annotations = whiteSpace >> pAnnotation `manyTill` eof

pAnnotation
    :: (MonadIO m, MonadWriter Pm.RenderingLog m, MonadReader Pm.RenderingParameters m)
    => ParsecT String u m Annotation
pAnnotation = (try (annotation <$> car)
    <|> try (annotation <$> seg)
    <|> try (annotation <$> splitSeg)
    <|> try (annotation <$> standaloneCaption)
    <|> try (annotation <$> traceSpec SingleFrameTrace)
    ) <* annDelimiter

annDelimiter :: Stream s m Char => ParsecT s u m ()
annDelimiter = void (detectAnnStart <|> try semi) <|> eof

detectAnnStart :: Stream s m Char => ParsecT s u m String
detectAnnStart = choice . map (lookAhead . try . symbol) $
    ["Car", "X", "Circle", "Diamond", "Dot", "Arrow", "Text", "Seg", "Split", "Trace"]

car :: Monad m => ParsecT String u m CarAnnotation
car = do
    spr <- sprite
    opt <- runPermParser $
        (,,,,,,,) <$> oncePerm xy
                  <*> optionMaybePerm colour
                  <*> optionMaybePerm bg
                  <*> optionMaybePerm angle
                  <*> optionMaybePerm size
                  <*> optionMaybePerm lineWidth
                  <*> optionMaybePerm invert
                  <*> optionMaybePerm caption
    let (pos, cl, bg, ang, sz, mWid, inv, capt) = opt
    return $ maybe id deepOverrideAnnColour cl $ defAnn
        & carAnnPosition .~ pos
        & maybe id (carAnnAngle .~) ang
        & maybe id (carAnnSize .~) sz
        & carAnnLineWidth .~ mWid
        & maybe id (carAnnInvert .~) inv
        & maybe id (carAnnCaption .~) capt
        & maybe id (carAnnOpacity .~) bg
        & carAnnSprite .~ spr

sprite :: Stream s m Char => ParsecT s u m CarSprite
sprite = try (Acura <$ symbol "Car")
    <|> try (XMarker <$ symbol "X")
    <|> try (CircleMarker <$ symbol "Circle")
    <|> try (DiamondMarker <$ symbol "Diamond")
    <|> try (DotMarker <$ symbol "Dot")
    <|> try (ArrowMarker <$ symbol "Arrow")

seg :: Monad m => ParsecT String u m SegAnnotation
seg = do
    symbol "Seg"
    opt <- runPermParser $
        (,,,,,) <$> oncePerm xy
                <*> optionMaybePerm colour
                <*> oncePerm angle
                <*> oncePerm size
                <*> optionMaybePerm lineWidth
                <*> optionMaybePerm caption
    let (pos, mCl, ang, len, mWid, capt) = opt
    return $ maybe id deepOverrideAnnColour mCl $ defAnn
        & segAnnPosition .~ pos
        & segAnnAngle .~ ang
        & segAnnLength .~ len
        & segAnnWidth .~ mWid
        & maybe id (segAnnCaption .~) capt

splitSeg :: Monad m => ParsecT String u m SplitAnnotation
splitSeg = do
    symbol "Split"
    ix <- fromIntegral <$> integer
    opt <- runPermParser $
        (,,,,,,,) <$> oncePerm xyInt
                  <*> optionMaybePerm colour
                  <*> optionMaybePerm bg
                  <*> oncePerm splitDir
                  <*> oncePerm sizeInt
                  <*> optionMaybePerm lineWidth
                  <*> optionMaybePerm alignment
                  <*> optionMaybePerm invert
    let (pos, cl, captBg, splD, len, mWid, captAl, captInv) = opt
    return $ defAnn
        & maybe id (splAnnColour .~) cl
        & splAnnIndex .~ ix
        & splAnnPosition .~ pos
        & splAnnDirection .~ splD
        & splAnnLength .~ len
        & maybe id (splAnnWidth .~) mWid
        & maybe id (splAnnCaptBgOpacity .~) captBg
        & splAnnCaptAlignment .~ fromMaybe splD captAl
        & maybe id (splAnnCaptInvert .~) captInv

xy :: Stream s m Char => ParsecT s u m (Double, Double)
xy = do
    symbol "@"
    x <- floatOrInteger
    -- Separation with spaces is implied.
    y <- floatOrInteger
    -- No bounds checking, as using space beyond the map might be useful.
    return (x, y)

xyInt :: (Stream s m Char, Num a) => ParsecT s u m (a, a)
xyInt = do
    symbol "@"
    x <- fromIntegral <$> integer
    y <- fromIntegral <$> integer
    return (x, y)

angle :: Stream s m Char => ParsecT s u m Double
angle = do
    symbol "^"
    floatOrInteger

-- TODO: add triplet support (see Data.Colour.SRGB)
-- It is convenient that readColourName fails with fail.
colour :: (Stream s m Char, Floating b, Ord b) => ParsecT s u m (Colour b)
colour = do
    symbol "#"
    try (replicateM 6 hexDigit >>= ((skipMany space >>) . return . sRGB24read))
        <|> (many1 alphaNum >>= ((skipMany space >>) . readColourName))

-- For the moment, this only parses the background opacity.
bg :: Stream s m Char => ParsecT s u m Double
bg = do
    symbol "$"
    (/100) <$> floatOrInteger

size :: Stream s m Char => ParsecT s u m Double
size = do
    symbol "*"
    sz <- floatOrInteger
    case sz of
        0 -> fail "Annotation size must not be zero"
        _ -> return sz

lineWidth :: Stream s m Char => ParsecT s u m Double
lineWidth = do
    symbol ">"
    floatOrInteger

sizeInt :: (Stream s m Char, Num b) => ParsecT s u m b
sizeInt = do
    symbol "*"
    fromIntegral <$> integer

invert :: Stream s m Char => ParsecT s u m Bool
invert = do
    symbol "%"
    return True

-- Captions associated to another annotation.
caption :: Monad m => ParsecT String u m CaptAnnotation
caption = do
    txt <- stringLiteral
    mOpt <- optionMaybe . try . braces $
        runPermParser $ (,,,,,) <$> optionMaybePerm alignment
                                <*> optionMaybePerm size
                                <*> optionMaybePerm angle
                                <*> optionMaybePerm invert
                                <*> optionMaybePerm colour
                                <*> optionMaybePerm bg
    let setCapt = maybe id $ \(mAl, mSz, mAng, mInv, mCl, mBg) ->
            maybe id customiseAnnColour mCl
            . maybe id (captAnnAlignment .~) mAl
            . maybe id (captAnnSize .~) mSz
            . maybe id (captAnnAngle .~) mAng
            . maybe id (captAnnInvert .~) mInv
            . maybe id (captAnnBgOpacity .~) mBg
    return $ setCapt mOpt $ defAnn & captAnnText .~ txt

-- Standalone captions.
standaloneCaption :: Stream s m Char => ParsecT s u m CaptAnnotation
standaloneCaption = do
    symbol "Text"
    txt <- stringLiteral
    opt <- runPermParser $
        (,,,,,) <$> oncePerm xy
                <*> optionMaybePerm size
                <*> optionMaybePerm angle
                <*> optionMaybePerm invert
                <*> optionMaybePerm colour
                <*> optionMaybePerm bg
    let setCapt = \(pos, mSz, mAng, mInv, mCl, mBg) ->
            maybe id customiseAnnColour mCl
            . maybe id (captAnnSize .~) mSz
            . maybe id (captAnnAngle .~) mAng
            . maybe id (captAnnInvert .~) mInv
            . maybe id (captAnnBgOpacity .~) mBg
            . (captAnnPosition .~ pos)
    return $ setCapt opt $ defAnn & captAnnText .~ txt

cardinalDir :: Monad m => String -> ParsecT String u m CardinalDirection
cardinalDir leading = do
    symbol leading
    read <$> (
        try (symbol "E")
        <|> try (symbol "N")
        <|> try (symbol "W")
        <|> symbol "S")

alignment :: Monad m => ParsecT String u m CardinalDirection
alignment = cardinalDir "'"

splitDir :: Monad m => ParsecT String u m CardinalDirection
splitDir = cardinalDir "^"


traceSpec
    :: (MonadIO m, MonadWriter Pm.RenderingLog m, MonadReader Pm.RenderingParameters m)
    => TraceMode -> ParsecT String u m TraceAnnotation
traceSpec traceMode = do
    symbol "Trace"
    opt <- runPermParser $
        (,,,,,) <$> oncePerm rawPath
                <*> optionMaybePerm colour
                <*> optionMaybePerm visibility
                <*> optionMaybePerm lineWidth
                <*> manyPerm (onTrace $ carOnTrace Nothing)
                <*> optionMaybePerm (periodic $ carOnTrace (Just 0))
    let (path, mCl, mVis, mWid, cars, mPCars) = opt
    basePath <- lift $ asks Pm.baseDirectory
    let fullPath = basePath </> path
    exists <- liftIO $ doesFileExist fullPath
    if not exists
        then fail "Trace data file does not exist."
        else do
            rawData <- liftIO $ readFile fullPath
            let eDat = runP laptrace () fullPath rawData
            case eDat of
                Left e    -> fail $ show e
                Right dat -> return $ initializeTrace traceMode dat
                    . maybe id deepOverrideAnnColour mCl
                    . maybe id (traceAnnVisible .~) mVis
                    . maybe id (traceAnnWidth .~) mWid
                    . maybe id (\pc -> L.over traceAnnOverlays
                        (periodicCarsSpec .~ pc)) mPCars
                    $ defAnn
                        & traceAnnOverlays .~ (defAnn & carsOverTrace .~ cars)

-- As the name indicates, the "path" could be anything.
rawPath :: Stream s m Char => ParsecT s u m FilePath
rawPath = do
    symbol ":"
    stringLiteral

visibility :: Stream s m Char => ParsecT s u m Bool
visibility = do
    symbol "!"
    return False

lapMoment :: Stream s m Char => ParsecT s u m Double
lapMoment = do
    symbol "@"
    floatOrInteger

momentToFrame :: Double -> Int
momentToFrame = truncate . (20 *)

-- Trace overlays.
onTrace :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
onTrace ovr = do
    symbol "+"
    braces ovr

-- Specification for overlays spread periodically over a trace.
periodic
  :: Stream s m Char
  => ParsecT s u m (x, CarAnnotation, [CaptAnnotation])
  -> ParsecT s u m PeriodicCarsSpec
periodic ovr = do
    symbol "~"
    ifr <- momentToFrame <$> floatOrInteger
    freq <- momentToFrame <$> floatOrInteger
    baseCar <- braces ovr
    fbkCapts <- many flipbookCaption
    return $ def
        & periodicInitialFrame .~ ifr
        & periodicPeriod .~ freq
        & periodicBaseCar .~ L.view L._2 baseCar
        -- TODO: Parsed standalone frame-bound captions associated to a
        -- periodic annotation are ignored, because there is no useful
        -- way to render them in a single picture (for a flipbook, we
        -- use the result of flipbookCaption instead, which is sent to
        -- a different pipeline). It would be reasonable to rearrange
        -- the parsers so that these specifications aren't silently
        -- ignored.
        -- & periodicStandaloneCaptions .~ L.view L._3 baseCar
        & periodicFlipbookCaptions .~ fbkCapts

-- TODO: Minimize duplication in the car parsers.
carOnTrace
    :: Monad m
    => Maybe Double
    -> ParsecT String u m (FrameIndex, CarAnnotation, [CaptAnnotation])
carOnTrace mMoment = do
    spr <- sprite
    opt <- runPermParser $
        (,,,,,,,) <$> maybe (oncePerm lapMoment) (oncePerm . return) mMoment
                  <*> optionMaybePerm colour
                  <*> optionMaybePerm bg
                  <*> optionMaybePerm size
                  <*> optionMaybePerm lineWidth
                  <*> optionMaybePerm invert
                  <*> optionMaybePerm caption
                  <*> manyPerm flipbookCaption
    let (moment, mCl, bg, sz, mWid, inv, capt, saCapts) = opt
    return $ (momentToFrame moment
        , maybe id deepOverrideAnnColour mCl $ defAnn
            & maybe id (carAnnSize .~) sz
            & carAnnLineWidth .~ mWid
            & maybe id (carAnnInvert .~) inv
            & maybe id (carAnnCaption .~) capt
            & maybe id (carAnnOpacity .~) bg
            & carAnnSprite .~ spr
        , saCapts)

-- Frame-bound standalone captions, both for individual and periodic
-- flipbook overlays.
flipbookCaption :: Stream s m Char => ParsecT s u m CaptAnnotation
flipbookCaption = do
    symbol "&"
    braces standaloneCaption

-- Flipbook parsers.
parseFlipbook
    :: (MonadIO m, MonadWriter Pm.RenderingLog m, MonadReader Pm.RenderingParameters m)
    => String -> m [SomeFlipbook]
parseFlipbook input = do
    result <- runPT flipbookSpec () "" input
    case result of
        Left err   -> do
            tell . Pm.logFromString $
                "Error in defining the animation "
            tell . Pm.logFromString . show $ err
            tell . Pm.logFromString $ "\r\n\r\n"
            return def
        Right fbks -> return fbks

flipbookSpec
    :: (MonadIO m, MonadWriter Pm.RenderingLog m, MonadReader Pm.RenderingParameters m)
    => ParsecT String u m [SomeFlipbook]
flipbookSpec = whiteSpace >> many (SomeFlipbook <$> traceSpec FlipbookTrace)

floatOrInteger :: Stream s m Char => ParsecT s u m Double
floatOrInteger = try float <|> fromIntegral <$> integer

lexer :: Stream s m Char => P.GenTokenParser s u m
lexer = P.makeTokenParser annLang

-- Mostly copied from the doc; doesn't really matter for now.
annLang :: Stream s m Char => P.GenLanguageDef s u m
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

float :: Stream s m Char => ParsecT s u m Double
float = P.float lexer

integer :: Stream s m Char => ParsecT s u m Integer
integer = P.integer lexer

stringLiteral :: Stream s m Char => ParsecT s u m String
stringLiteral = P.stringLiteral lexer

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = P.symbol lexer

braces :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
braces = P.braces lexer

parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = P.parens lexer

semi :: Stream s m Char => ParsecT s u m String
semi = P.semi lexer

whiteSpace :: Stream s m Char => ParsecT s u m ()
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
