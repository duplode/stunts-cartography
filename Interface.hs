module Main where

import Control.Monad
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
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
    buttonGo <- mkButtonGo
    trackPng <- loadTrackPng w
    imgTrack <- mkImgTrack # set UI.src trackPng
    getBody w #+
        [ UI.div #. "wrap-go" #+ map element [buttonGo]
        , UI.div #. "wrap-map" #+ map element [imgTrack]
        ]

mkImgTrack :: IO Element
mkImgTrack = UI.img #. "track-map" # set UI.id_ "track-map"

mkButtonGo :: IO Element
mkButtonGo = do
    button <- UI.button #. "button" #+ [string "Go!"]
    on UI.click button $ \_ -> do
        w <- fromJust <$> getWindow button --TODO: ick
        writePngOutput
        trackPng <- loadTrackPng w
        (fromJust <$> getElementById w "track-map") # set UI.src trackPng
    return button

loadTrackPng :: Window -> IO String
loadTrackPng w = loadFile w "image/png" "./test.png"
