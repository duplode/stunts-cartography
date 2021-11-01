{-# LANGUAGE RecordWildCards #-}
module Dump.GameState
    ( GameState(..)
    , CarState(..)
    , parseStates
    , parseFile
    , rot
    ) where

import Dump.Common

import qualified Data.Binary.Get as B
import qualified Data.ByteString.Lazy as BS

data GameState = GameState
    { gsData1 :: BS.ByteString
    , v1 :: Vec
    , v2 :: Vec
    , v3 :: Vec
    , v4 :: Vec
    , fis :: Int
    , fps :: Int
    , dist :: Integer
    , frame :: Int
    , unk1 :: Int
    , unk2 :: Int
    , playerFinish :: Bool
    , oppFinish :: Bool
    , fpsCount :: Int
    , impactSpeed :: Int
    , topSpeed :: Int
    , jumps :: Int
    , player :: CarState
    , opponent :: CarState
    , gsData2 :: BS.ByteString
    } deriving (Show)

data CarState = CarState
    { curPos :: VecWide  -- Formerly pos1
    , lastPos :: VecWide  -- Formerly pos2
    , rotXZ :: Int
    , rotYZ :: Int
    , rotXY :: Int
    , grav :: Int
    , steer :: Int
    , curRpm :: Int
    , lastRpm :: Int
    , idleRpm :: Int
    , speedDiff :: Int
    , coupledSpeed :: Int  -- Formerly speed
    , curSpeed :: Int  -- Formerly speed2
    , lastSpeed :: Int
    , rawGearRatio :: Int
    , gearRatio :: Int
    , knobX :: Int
    , whlAngle36 :: Int
    , knobY :: Int
    , knobX2 :: Int
    , knobY2 :: Int
    , angleZ :: Int
    , frontWhlAngle40 :: Int
    , field42 :: Int
    , demandedGrip :: Int
    , surfaceGripSum :: Int
    , field48 :: Int
    , csData1 :: BS.ByteString
    , curGear :: Int
    , csData2 :: BS.ByteString
    } deriving (Show)

getGameState :: B.Get GameState
getGameState = do
    gsData1 <- getUnstructured 0x120
    v1 <- getVec16
    v2 <- getVec16
    v3 <- getVec16
    v4 <- getVec16
    fis <- getUint16
    fps <- getUint16
    dist <- getUint32
    frame <- getUint16
    unk1 <- getUint16
    unk2 <- getUint16
    playerFinish <- getBool16
    oppFinish <- getBool16
    fpsCount <- getUint16
    impactSpeed <- getUint16
    topSpeed <- getUint16
    jumps <- getUint16
    player <- getCarState
    opponent <- getCarState
    gsData2 <- getUnstructured 0x16E

    return $ GameState {..}

getCarState :: B.Get CarState
getCarState = do
    curPos <- getVec32
    lastPos <- getVec32
    rotXZ <- getSint16
    rotYZ <- getSint16
    rotXY <- getSint16
    grav <- getSint16
    steer <- getSint16
    curRpm <- getUint16
    lastRpm <- getUint16
    idleRpm <- getUint16
    speedDiff <- getSint16
    coupledSpeed <- getUint16
    curSpeed <- getUint16
    lastSpeed <- getUint16
    rawGearRatio <- getUint16
    gearRatio <- getUint16
    knobX <- getUint16
    whlAngle36 <- getSint16
    knobY <- getUint16
    knobX2 <- getUint16
    knobY2 <- getUint16
    angleZ <- getUint16
    frontWhlAngle40 <- getSint16
    field42 <- getUint16
    demandedGrip <- getUint16
    surfaceGripSum <- getUint16
    field48 <- getSint16
    csData1 <- getUnstructured 0x74
    curGear <- getUint8
    csData2 <- getUnstructured 0x11

    return $ CarState {..}

-- Convenience function.
rot :: CarState -> Vec
rot cs = (rotXZ cs, rotYZ cs, rotXY cs)

parseStates :: BS.ByteString -> [GameState]
parseStates = B.runGet $ do
    nFrames <- getUint16
    sequence . replicate nFrames $ getGameState
    --nLeft <- B.remaining
    --trace (show nLeft) $ return gss

parseFile :: FilePath -> IO [GameState]
parseFile path = parseStates <$> BS.readFile path

