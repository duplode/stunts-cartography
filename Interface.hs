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
    return w # set title "Vectorial Track Viewer"
    fileInput <- mkFileInput
    buttonGo <- mkButtonGo
    trackPng <- loadTrackPng w
    imgTrack <- mkImgTrack # set UI.src trackPng
    getBody w #+
        [ UI.div #. "wrap-go" #+ map element [fileInput, buttonGo]
        , UI.div #. "wrap-map" #+ map element [imgTrack]
        ]

mkImgTrack :: IO Element
mkImgTrack = UI.img #. "track-map" # set UI.id_ "track-map"

mkFileInput :: IO Element
mkFileInput = UI.input # set UI.id_ "trk-input" -- # set UI.type_ "file"

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

