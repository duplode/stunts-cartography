module Main
    ( main
    ) where

import System.FilePath
import System.Directory
import System.Environment (getArgs)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Control.Arrow
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

coordsToTextSimple :: [GameState] -> Text
coordsToTextSimple gss =
    returnA
    >>> map (pos1 . player) &&& map (rot . player)
    >>> map textFrom3D *** map textFrom3D
    >>> id *** map (T.cons '\t')
    >>> arr (uncurry $ zipWith T.append)
    >>> arr T.unlines
        $ gss
