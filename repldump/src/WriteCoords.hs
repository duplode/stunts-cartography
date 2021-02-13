module Main
    ( main
    ) where

import Control.Arrow
import System.FilePath
import System.Directory
import System.Environment (getArgs)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import GameState

main = do
    args <- getArgs
    case args of
        []    -> putStrLn "Please specify at least one file."
        paths -> mapM_ writeCoords paths

writeCoords :: FilePath -> IO ()
writeCoords path = do
    exists <- doesFileExist path
    if not exists
        then putStrLn $ path ++ " does not exist."
        else do
            let outPath = path `replaceExtension` ".dat"
            parseFile path >>= T.writeFile outPath . coordsToTextSimple

textFrom3D :: (Show a) => (a, a, a) -> Text
textFrom3D (x, y, z) = T.intercalate (T.pack "\t") $
    map (T.pack . show) [x, y, z]

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
alignAngleToGrid ang = 256 * (quad + dif `div` 128)
    where
    (quad, dif) = ang `divMod` 256

-- TODO: Rewrite this in a more sensible manner.
coordsToTextSimple :: [GameState] -> Text
coordsToTextSimple gs =
    returnA
    >>> map curPos
        &&& (map rot
            &&& (map curSpeed
                &&& map curGear))
    >>> map textFrom3D
        *** (map textFrom3D
            *** (map (T.pack . show)
                *** map (T.pack . show)))
    >>> id
        *** (map (T.cons '\t')
            *** (map (T.cons '\t')
                *** map (T.cons '\t')))
    >>> second (second (arr (uncurry $ zipWith T.append)))
    >>> second (arr (uncurry $ zipWith T.append))
    >>> arr (uncurry $ zipWith T.append)
    >>> arr T.unlines
        $ forgeInitialCarState (map player gs)
