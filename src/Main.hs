module Main
    ( main
    ) where

import qualified Viewer as Viewer
import qualified Repldump as Repldump
import qualified Trackdata as Trackdata
import Paths (versionString)

import qualified Options.Applicative as Opts

import Control.Applicative
import Text.Printf

main :: IO ()
main = do
    fullOpts <- Opts.customExecParser p outerOpts
    case fullOpts of
        Viewer o -> Viewer.subMain o
        Repldump o -> Repldump.subMain o
        Trackdata o -> Trackdata.subMain o
    where
    p = Opts.prefs (Opts.showHelpOnError <> Opts.showHelpOnEmpty)

data Command
    = Viewer Viewer.Options
    | Repldump Repldump.Options
    | Trackdata Trackdata.Options

outerOpts :: Opts.ParserInfo Command
outerOpts = Opts.info (commandOpts <**> Opts.helper <**> optVersion)
    ( Opts.fullDesc
    <> Opts.progDesc "Power tools for creating Stunts track maps"
    )
    where
    commandOpts = Opts.hsubparser
        ( Opts.command "viewer" (Viewer <$> Viewer.opts)
        <> Opts.command "r2c" (Repldump <$> Repldump.opts)
        <> Opts.command "t2c" (Trackdata <$> Trackdata.opts)
        )
    optVersion = Opts.infoOption formattedVersionString
        (Opts.long "version" <> Opts.help "Print version information")

formattedVersionString :: String
formattedVersionString = printf "Stunts Cartography %s"
    (maybe "" id versionString)
