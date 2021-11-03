-- This is the would-be Main module of repldump2carto, formerly known
-- as WriteCoords.
module Repldump
    ( subMain
    , Options (..)
    , opts
    ) where

import Control.Arrow
import System.FilePath
import System.Directory
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Options.Applicative as Opts
import qualified Options.Applicative.NonEmpty as Opts

import Dump.GameState
import Dump.WriteCoords

subMain :: Options -> IO ()
subMain o = do
    let Options { inputFiles = paths, carToFollow = follow } = o
    mapM_ (writeCoords follow) paths

data Options = Options
    { inputFiles :: NonEmpty FilePath
    , carToFollow :: CarToFollow
    }

data CarToFollow = Player | Opponent

-- TODO: Use opponentFlag once we get repldump to export opponent data.
baseOpts :: Opts.Parser Options
baseOpts = Options <$> argsFiles <*> pure Player

argsFiles :: Opts.Parser (NonEmpty FilePath)
argsFiles = Opts.some1 ((Opts.argument Opts.str)
    ( Opts.help "Binary repldump output files"
    <> Opts.metavar "FILES..."
    ))

opponentFlag :: Opts.Parser CarToFollow
opponentFlag = Opts.flag Player Opponent
    ( Opts.short 't'
    <> Opts.long "opponent"
    <> Opts.help "Generate opponent trace"
    )

opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts
    ( Opts.fullDesc
    <> Opts.progDesc "Convert repldump output to Cartography trace input"
    )

writeCoords :: CarToFollow -> FilePath -> IO ()
writeCoords follow path = do
    exists <- doesFileExist path
    if not exists
        then putStrLn $ path ++ " does not exist."
        else do
            gss <- parseFile path
            let outPath = path `replaceExtension` ".dat"
            T.writeFile outPath (coordsToText follow gss)

-- As it stands, repldump doesn't include the initial 0:00.00 frame in
-- its output. That leaves us with two options: either follow suit and
-- skip the initial frame, or forge it using the second frame data. The
-- latter option isn't as awkward as it sounds. Many fields of interest
-- are already available for the previous frame through CarState, some
-- other fields can be easily extrapolated considering the car doesn't
-- move between 0:00.00 and 0:00.05, and in any case we always have the
-- option of pre-defining an initial value based on the restunts notes.
-- We will probably want to do that anyway once we turn our eyes to
-- GameState.
--
-- Naturally, the extrapolation here should be reviewed carefully every
-- time we add a new field to the repldump2carto output.
forgeInitialCarState :: [CarState] -> [CarState]
forgeInitialCarState cs = case cs of
    [] -> []
    c : _ -> forge c : cs
    where
    forge c = c
        { curPos = lastPos c  -- Not unchanged, though the difference
                              -- will often be less than one graphical
                              -- unit.
        , rotXZ = alignAngleToGrid (rotXZ c)
        , rotYZ = alignAngleToGrid (rotYZ c)
        , rotXY = alignAngleToGrid (rotXY c)
        , curRpm = 0
        , speedDiff = 0
        , coupledSpeed = 0
        , curSpeed = 0
        -- TODO: Except for the current gear, fields involving
        -- transmission and steering aren't being handled.
        -- Changes to those fields over the first timestep,
        -- while unlikely, can't be ruled out without further
        -- investigation.
        , curGear = 1
        }

-- Making sure the forged initial frame orientations are parallel to the
-- axes. This function might get it wrong if there are angles near
-- 45 degrees in the second frame. That is extremely unlikely to happen
-- in practice, though.
alignAngleToGrid :: Int -> Int
alignAngleToGrid ang = 256 * (quad + dif `div` 128)
    where
    (quad, dif) = ang `divMod` 256

coordsToText :: CarToFollow -> [GameState] -> Text
coordsToText follow = T.unlines . map (printAsRow cells) . setup
    where
    carSelector = case follow of
        Player -> player
        Opponent -> opponent
    setup = forgeInitialCarState . map carSelector
    cells =
        [ textFrom3D . curPos
        , textFrom3D . rot
        , tShow . curSpeed
        , tShow . curGear
        ]
