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

-- Toy bindings to alertify.js 0.3 API.

-- Synchronously loading the library code and CSS.
-- If you find this hack off-putting, an alternative is to specify them
-- through the tpCustomHTML file in the Threepenny configuration.
-- TODO: Decide whether to use extra subdirectories here.
alertifySetup :: Window -> FilePath -> UI ()
alertifySetup w libDir = do
    let jsPath = libDir ++ "alertify.min.js"
        addScriptTag :: JSFunction ()
        addScriptTag = ffi $ unlines
            [ "(function(){"
            , "var xhrObj = new XMLHttpRequest();"
            , "xhrObj.open('GET', '" ++ jsPath ++ "', false);"
            , "xhrObj.send('');"
            , "var script = document.createElement('script');"
            , "script.type = 'text/javascript';"
            , "script.id = 'jquery-ac';"
            , "script.text = xhrObj.responseText;"
            , "document.getElementsByTagName('head')[0].appendChild(script);"
            , "return null;"
            , "})();"
            ]
    callFunction addScriptTag
    mapM_ (UI.addStyleSheet w) ["alertify.core.css", "alertify.default.css"]

data LogType = StandardLog | SuccessLog | ErrorLog | CustomLog String
    deriving (Show)

alertifyLog' :: String -> LogType -> Int -> UI ()
alertifyLog' msg type_ timeout = runFunction $
    ffi "alertify.log(%1, %2, %3);" msg strType timeout
    where
    strType = case type_ of
        StandardLog      -> "standard"
        SuccessLog       -> "success"
        ErrorLog         -> "error"
        CustomLog class_ -> class_

alertifyLog :: String -> UI ()
alertifyLog msg = runFunction $ ffi "alertify.log(%1);" msg

alertifySuccess :: String -> UI ()
alertifySuccess msg = runFunction $ ffi "alertify.success(%1);" msg

alertifyError :: String -> UI ()
alertifyError msg = runFunction $ ffi "alertify.error(%1);" msg
