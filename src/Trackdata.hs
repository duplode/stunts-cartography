{-# LANGUAGE LambdaCase #-}
module Trackdata
    ( subMain
    , Options (..)
    , opts
    ) where

import Dump.Trackdata

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow
import Control.Applicative
import System.FilePath
import System.Directory
import qualified Options.Applicative as Opts
import qualified Options.Applicative.NonEmpty as Opts

subMain :: Options -> IO ()
subMain o = do
    let Options { inputFiles = paths, tdChoice = cho } = o
    mapM_ (writeCoords cho) paths

data Options = Options
    { tdChoice :: TdChoice
    , inputFiles :: NonEmpty FilePath
    }

data TdChoice = Td09 | Td10

baseOpts :: Opts.Parser Options
baseOpts = Options <$> flagTdChoice <*> argsFiles

argsFiles :: Opts.Parser (NonEmpty FilePath)
argsFiles = Opts.some1 ((Opts.argument Opts.str)
    ( Opts.help "Binary trackdata dump files"
    <> Opts.metavar "FILES..."
    ))

-- TODO: Change this to an option that parses a valid number.
flagTdChoice :: Opts.Parser TdChoice
flagTdChoice
    = Opts.flag' Td09
    ( Opts.long "09"
    <> Opts.help "Export trackdata09 (F4 camera positions)"
    )
    <|> Opts.flag' Td10
    ( Opts.long "10"
    <> Opts.help "Export trackdata10 (corner sign positions)"
    )


opts :: Opts.ParserInfo Options
opts = Opts.info baseOpts
    ( Opts.fullDesc
    <> Opts.progDesc "Convert a trackdata dump to Cartography trace input"
    )

-- This will eventually have to be generalised, once we begin exporting
-- a wider range of trackdata.
chooseTd :: TdChoice -> Trackdata -> [Vec]
chooseTd = \case
    Td09 -> trackdata09
    Td10 -> trackdata10

tdExtension :: TdChoice -> String
tdExtension = \case
    Td09 -> "td09"
    Td10 -> "td10"

textFrom3D :: (Show a) => (a, a, a) -> Text
textFrom3D (x, y, z) = T.intercalate (T.pack "\t") $
    map (T.pack . show) [x, y, z]

scale3DVec :: Num a => a -> (a, a, a) -> (a, a, a)
scale3DVec q (x, y, z) = (q*x, q*y, q*z)

coordsToTextSimple :: TdChoice -> Trackdata -> Text
coordsToTextSimple cho td =
    let tdx = chooseTd cho
    in returnA
    >>> (scale3DVec 64 <$>) . tdx
        &&& ((0, 0, 0) <$) . tdx
            &&& (0 <$) . tdx
                &&& (0 <$) . tdx
    >>> map textFrom3D
        *** map textFrom3D
            *** map (T.pack . show)
                *** map (T.pack . show)
    >>> id
        *** map (T.cons '\t')
            *** map (T.cons '\t')
                *** map (T.cons '\t')
    >>> second (second (arr (uncurry $ zipWith T.append)))
    >>> second (arr (uncurry $ zipWith T.append))
    >>> arr (uncurry $ zipWith T.append)
    >>> arr T.unlines
        $ td

writeCoords :: TdChoice -> FilePath -> IO ()
writeCoords cho path = do
    exists <- doesFileExist path
    if not exists
        then putStrLn $ path ++ " does not exist."
        else do
            td <- parseFile path
            let outPath = path `replaceExtension` (tdExtension cho <> ".dat")
            T.writeFile outPath (coordsToTextSimple cho td)
