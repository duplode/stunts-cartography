module Main where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import Control.Exception (handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Output



main :: IO ()
main = do
    startGUI Config
        { tpPort = 10000
        , tpCustomHTML = Nothing
        , tpStatic = "."
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Yet Another Track Viewer"
    UI.addStyleSheet w "viewer.css"
    trackPng <- loadTrackPng w
    getBody w #+
        [ UI.div #. "left-bar" #+
            [ UI.p #+ [string ".TRK file path:"]
            , UI.input # set UI.type_ "text" # set UI.name "trk-input"
                # set UI.id_ "trk-input"
            , mkButtonGo
            ]
        , UI.div #. "main-wrap" #+
            [ UI.img # set UI.id_ "track-map"]
        ]

mkButtonGo :: IO Element
mkButtonGo = do
    button <- UI.button #. "button" #+ [string "Go!"]
    on UI.click button $ \_ -> do
        w <- fromJust <$> getWindow button --TODO: ick
        trkPath <- join $ get value . fromJust
            <$> getElementById w "trk-input"
        trkExists <- doesFileExist trkPath
        mFileSize <- retrieveFileSize trkPath
        let sizeIsCorrect = mFileSize == Just 1802
        when (trkExists && sizeIsCorrect) $ writePngOutput trkPath
        trackPng <- loadTrackPng w
        (fromJust <$> getElementById w "track-map") # set UI.src trackPng
        --getBody w #+ [UI.p #. "message" #+ [string trkPath]]
    return button

loadTrackPng :: Window -> IO String
loadTrackPng w = loadFile w "image/png" "./test.png"

--Lifted from RWH chapter 9.
retrieveFileSize :: FilePath -> IO (Maybe Integer)
retrieveFileSize path = handle nothingHandler $ do
    h <- openFile path ReadMode
    size <- hFileSize h
    hClose h
    return (Just size)
    where
    nothingHandler :: IOError -> IO (Maybe Integer)
    nothingHandler = \_ -> return Nothing

