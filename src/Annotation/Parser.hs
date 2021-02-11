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
import Text.Parsec.Prim (runP)
import qualified Text.Parsec.Token as P
import Text.Parsec.Permutation

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*), (<*>))
import Data.Maybe (fromMaybe)
import Control.Monad.Trans
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Data.Default

import Annotation
import Annotation.LapTrace
import Annotation.LapTrace.Vec (VecDouble)
import Annotation.LapTrace.Parser.Simple
import Annotation.Flipbook
import Data.Colour (Colour)
import Data.Colour.Names (readColourName, yellow)
import Data.Colour.SRGB (sRGB24read)
import Types.CartoM
import Control.Monad.RWS (tell, asks)
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

pAnnotation = (try (annotation <$> car "Car" Acura)
    <|> try (annotation <$> car "X" XMarker)
    <|> try (annotation <$> car "Circle" CircleMarker)
    <|> try (annotation <$> car "Diamond" DiamondMarker)
    <|> try (annotation <$> car "Dot" DotMarker)
    <|> try (annotation <$> car "Arrow" ArrowMarker)
    <|> try (annotation <$> seg)
    <|> try (annotation <$> splitSeg)
    <|> try (annotation <$> standaloneCaption)
    <|> try (annotation <$> traceSpec SingleFrameTrace)
    ) <* annDelimiter

annDelimiter = ((detectAnnStart <|> try semi) >> return ()) <|> eof

detectAnnStart = choice . map (lookAhead . try . symbol) $
    ["Car", "X", "Circle", "Diamond", "Dot", "Arrow", "Text", "Seg", "Split", "Trace"]

car leadSym spr = do
    symbol leadSym
    opt <- runPermParser $
        (,,,,,,) <$> oncePerm xy
                 <*> optionMaybePerm colour
                 <*> optionPerm 1 bg
                 <*> optionPerm 0 angle
                 <*> optionPerm 0.5 size
                 <*> optionPerm False invert
                 <*> optionPerm defAnn caption -- The default caption is empty.
    let (pos, mCl, bg, ang, sz, inv, capt) = opt
    return $ maybeDeepOverrideAnnColour mCl $ defAnn
        & carAnnPosition .~ pos
        & carAnnAngle .~ ang
        & carAnnSize .~ sz
        & carAnnInvert .~ inv
        & carAnnCaption .~ capt
        & carAnnOpacity .~ bg
        & carAnnSprite .~ spr

seg = do
    symbol "Seg"
    opt <- runPermParser $
        (,,,,) <$> oncePerm xy
               <*> optionMaybePerm colour
               <*> oncePerm angle
               <*> oncePerm size
               <*> optionPerm defAnn caption
    let (pos, mCl, ang, len, capt) = opt
    return $ maybeDeepOverrideAnnColour mCl $ defAnn
        & segAnnPosition .~ pos
        & segAnnAngle .~ ang
        & segAnnLength .~ len
        & segAnnCaption .~ capt

splitSeg = do
    symbol "Split"
    ix <- fromIntegral <$> integer
    opt <- runPermParser $
        (,,,,,,) <$> oncePerm xyInt
                 <*> optionPerm yellow colour
                 <*> optionPerm 0 bg
                 <*> oncePerm splitDir
                 <*> oncePerm sizeInt
                 <*> optionMaybePerm alignment
                 <*> optionMaybePerm invert
    let (pos, cl, captBg, splD, len, mCaptAl, mCaptInv) = opt
    let captAl = fromMaybe splD mCaptAl
    return $ defAnn
        & splAnnColour .~ cl
        & splAnnIndex .~ ix
        & splAnnPosition .~ pos
        & splAnnDirection .~ splD
        & splAnnLength .~ len
        & splAnnCaptBgOpacity .~ captBg
        & splAnnCaptAlignment .~ fromMaybe splD mCaptAl
        & splAnnCaptInvert .~ fromMaybe False mCaptInv

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

invert = do
    symbol "%"
    return True

-- Captions associated to another annotation.
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
            maybeCustomiseAnnColour mCl
            . maybe id (captAnnAlignment .~) mAl
            . maybe id (captAnnSize .~) mSz
            . maybe id (captAnnAngle .~) mAng
            . maybe id (captAnnInvert .~) mInv
            . maybe id (captAnnBgOpacity .~) mBg
    return $ setCapt mOpt $ defAnn & captAnnText .~ txt

-- Standalone captions.
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
            maybeCustomiseAnnColour mCl
            . maybe id (captAnnSize .~) mSz
            . maybe id (captAnnAngle .~) mAng
            . maybe id (captAnnInvert .~) mInv
            . maybe id (captAnnBgOpacity .~) mBg
            . (captAnnPosition .~ pos)
    return $ setCapt opt $ defAnn & captAnnText .~ txt

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


traceSpec :: (MonadIO m) => TraceMode -> ParsecT String u (CartoT m) TraceAnnotation
traceSpec traceMode = do
    symbol "Trace"
    opt <- runPermParser $
        (,,,,) <$> oncePerm rawPath
               <*> optionMaybePerm colour
               <*> optionMaybePerm visibility
               <*> manyPerm (onTrace $ carOnTrace Nothing)
               <*> optionMaybePerm (periodic $ carOnTrace (Just 0))
    let (path, mCl, mVis, cars, mPCars) = opt
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
                    . maybeDeepOverrideAnnColour mCl
                    . maybe id (traceAnnVisible .~) mVis
                    . maybe id (\pc -> L.over traceAnnOverlays
                        (periodicCarsSpec .~ pc)) mPCars
                    $ defAnn
                        & traceAnnOverlays .~ (defAnn & carsOverTrace .~ cars)

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

momentToFrame :: Double -> Int
momentToFrame = truncate . (20 *)

-- Trace overlays. Only cars are supported for the time being.
onTrace ovr = do
    symbol "+"
    braces ovr

-- Specification for overlays spread periodically over a trace.
periodic ovr = do
    symbol "~"
    ifr <- momentToFrame <$> floatOrInteger
    freq <- momentToFrame <$> floatOrInteger
    baseCar <- braces ovr
    fbkCapt <- option defAnn flipbookCaption
    return $ def
        & periodicInitialFrame .~ ifr
        & periodicPeriod .~ freq
        & periodicBaseCar .~ snd baseCar
        & periodicFlipbookCaption .~ fbkCapt

-- TODO: Minimize duplication in the car parsers.
carOnTrace mMoment = do
    symbol "Car"
    opt <- runPermParser $
        (,,,,) <$> maybe (oncePerm lapMoment) (oncePerm . return) mMoment
               <*> optionMaybePerm colour
               <*> optionPerm 1 bg
               <*> optionPerm 0.5 size
               <*> optionPerm defAnn caption
    let (moment, mCl, bg, sz, capt) = opt
    return $ (momentToFrame moment
        , maybeDeepOverrideAnnColour mCl $ defAnn
            & carAnnSize .~ sz
            & carAnnCaption .~ capt
            & carAnnOpacity .~ bg
        )

flipbookCaption = do
    symbol "&"
    braces standaloneCaption

-- Flipbook parsers.
parseFlipbook :: (MonadIO m) => String -> CartoT m [SomeFlipbook]
parseFlipbook input = do
    result <- runPT flipbookSpec () "" input
    case result of
        Left err   -> do
            tell . Pm.logFromList $
                "Error in defining the animation "
            tell . Pm.logFromList . show $ err
            tell . Pm.logFromList $ "\r\n"
            return def
        Right fbks -> return fbks

flipbookSpec = whiteSpace >> many (SomeFlipbook <$> traceSpec FlipbookTrace)


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

