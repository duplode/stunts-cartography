{-# LANGUAGE LambdaCase #-}
module Trackdata
    ( subMain
    , Options (..)
    , opts
    ) where

import Dump.Trackdata
import Dump.Common (Vec)
import Dump.WriteCoords

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.List.NonEmpty (NonEmpty(..))
import Control.Arrow
import Control.Applicative
import System.FilePath
import System.Directory
import qualified Options.Applicative as Opts
import qualified Options.Applicative.NonEmpty as Opts
import Data.Functor.Contravariant

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

scale3DVec :: Num a => a -> (a, a, a) -> (a, a, a)
scale3DVec q (x, y, z) = (q*x, q*y, q*z)

coordsToTextSimple :: TdChoice -> Trackdata -> Text
coordsToTextSimple cho = T.unlines . map (getOp printer) . tdSelector
    where
    tdSelector = chooseTd cho
    adapt = scale3DVec 64 &&& const (0, 0, 0) &&& const 0 &&& const 0
    printer = adapt
        >$< textFrom3D
            `cglom` textFrom3D
                `cglom` tShow
                    `cglom` tShow

writeCoords :: TdChoice -> FilePath -> IO ()
writeCoords cho path = do
    exists <- doesFileExist path
    if not exists
        then putStrLn $ path ++ " does not exist."
        else do
            td <- parseFile path
            let outPath = path `replaceExtension` (tdExtension cho <> ".dat")
            T.writeFile outPath (coordsToTextSimple cho td)
    where
    tdExtension = \case
        Td09 -> "td09"
        Td10 -> "td10"

