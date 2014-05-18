{-# LANGUAGE OverloadedStrings #-}
module Util.Threepenny.JQueryAutocomplete
    ( autocompleteSetup
    , autocompleteInit
    , autocompleteArraySource
    , autocompleteAssocSource
    , autocompleteDelay
    , autocompleteMinLength
    , autocompletechange
    , autocompleteselect
    ) where

-- TODO: The module name is obviously provisional.
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Data.Aeson
import Data.Maybe (fromJust)
import Control.Monad (join)

-- Synchronously loading the library code and CSS.
-- If you find this hack off-putting, an alternative is to specify them
-- through the tpCustomHTML file in the Threepenny configuration.
autocompleteSetup :: Window -> FilePath -> UI ()
autocompleteSetup w libDir = do
    let jsPath = libDir ++ "jquery-ui-autocomplete/jquery-ui-1.10.4.custom.min.js"
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
    mapM_ (UI.addStyleSheet w)
        ["jquery-ui-autocomplete/ui-lightness/jquery-ui-1.10.4.custom.min.css"]

-- Initializes an autocomplete with no options set.
autocompleteInit :: UI Element -> UI Element
autocompleteInit el = do
    x <- el
    runFunction $ fun x
    el
    where
    fun :: Element -> JSFunction ()
    fun = ffi "$(%1).autocomplete();"

-- Note the ultra-lazy "serialization".
-- TODO: Implement the other sources, as well as a getter for source.
autocompleteArraySource :: WriteAttr Element [String]
autocompleteArraySource = mkWriteAttr $ \items x ->
    runFunction $ fun x (toJSON items)
    where
    fun :: Element -> Value -> JSFunction ()
    fun = ffi "$(%1).autocomplete(\"option\", \"source\", %2);"

autocompleteAssocSource :: WriteAttr Element [(String, String)]
autocompleteAssocSource = mkWriteAttr $ \items x ->
    runFunction $ fun x (toJSON $ pairsToItemObjects items)
    where
    fun :: Element -> Value -> JSFunction ()
    fun = ffi "$(%1).autocomplete(\"option\", \"source\", %2);"
    pairsToItemObjects :: [(String, String)] -> [Value]
    pairsToItemObjects = map $
        \(lab, val) -> object [("label", toJSON lab), ("value", toJSON val)]

-- Note that all of these attributes could just as well be ReadWriteAttr.
-- We are not bothering for now; one of the reasons being that the relevant
-- parts of the Threepenny API are set to change in 0.5.

-- acAutoFocus :: Attr Element Bool

autocompleteDelay :: WriteAttr Element Int
autocompleteDelay = mkWriteAttr $ \delay x ->
    runFunction $ fun x delay
    where
    fun :: Element -> Int -> JSFunction ()
    fun = ffi "$(%1).autocomplete(\"option\", \"delay\", %2);"

-- acDisabled :: Attr Element Bool -- see the enabled Threepenny attribute.

autocompleteMinLength :: WriteAttr Element Int
autocompleteMinLength = mkWriteAttr $ \minLength x ->
    runFunction $ fun x minLength
    where
    fun :: Element -> Int -> JSFunction ()
    fun = ffi "$(%1).autocomplete(\"option\", \"minLength\", %2);"

-- acPosition :: Attr Element JQUIPosition

-- TODO: Methods.

-- The events do not seem to give back useful data as far as EventData is
-- concerned. The commented signatures indicate the types we would expect
-- them to have.
-- autocompletechange :: Element -> Event (Maybe (String, String))
autocompletechange :: Element -> Event ()
autocompletechange = silence . domEvent "autocompletechange"

-- autocompletecreate :: Element -> Event ()
-- autocompletefocus :: Element -> Event (String, String)
-- autocompleteopen :: Element -> Event ()
-- autocompleteresponse Element -> Event [(String, String)]
-- autocompletesearch :: Element -> Event ()

-- autocompleteselect :: Element -> Event (String, String)
autocompleteselect :: Element -> Event ()
autocompleteselect = silence . domEvent "autocompleteselect"

silence :: (Functor f) => f a -> f ()
silence = fmap (const ())

{-
readJSObjectOptimistically :: String -> [(String, JSValue)]
readJSObjectOptimistically = fromJSObject
    . (\(Ok (JSObject o)) -> o)
    . decode

firstJSONEventResult :: EventData -> Maybe String
firstJSONEventResult ed = case ed of
    EventData []       -> Nothing
    EventData (mso : _) -> do
        o <- readJSObjectOptimistically <$> mso
        it <- lookup "item" o
        val <- lookup "value" $ fromJSObject . (\(JSObject o) -> o) $ it
        return $ (\(JSString s) -> fromJSString s) val
-}
