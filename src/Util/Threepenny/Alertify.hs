module Util.Threepenny.Alertify
    ( alertifySetup
    , LogType(..)
    , alertifyLog'
    , alertifyLog
    , alertifySuccess
    , alertifyError
    ) where

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import System.FilePath ((</>))

-- Toy bindings to alertify.js 0.3 API.

alertifySetup :: Window -> FilePath -> UI ()
alertifySetup w libDir = do
    scrAlertify <- mkElement "script" # set UI.src (libDir </> "alertify.min.js")
    getHead w #+ [element scrAlertify]
    mapM_ (UI.addStyleSheet w) ["alertify.core.css", "alertify.default.css"]

data LogType = StandardLog | SuccessLog | ErrorLog deriving (Show)

alertifyLog' :: String -> LogType -> Int -> UI ()
alertifyLog' msg type_ timeout = runFunction $
    ffi "alertify.log(%1, %2, %3)" msg strType timeout
    where
    strType = case type_ of
        StandardLog -> "standard"
        SuccessLog  -> "success"
        ErrorLog    -> "error"

alertifyLog :: String -> UI ()
alertifyLog msg = runFunction $ ffi "alertify.log(%1)" msg

alertifySuccess :: String -> UI ()
alertifySuccess msg = runFunction $ ffi "alertify.success(%1)" msg

alertifyError :: String -> UI ()
alertifyError msg = runFunction $ ffi "alertify.error(%1)" msg
