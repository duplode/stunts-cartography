module Util.Threepenny.JQueryAutocomplete
    ( autocompleteSetup
    , autocompleteInit
    , autocompleteArraySource
    , autocompleteDelay
    , autocompleteMinLength
    , autocompletechange
    , autocompleteselect
    ) where

-- TODO: The module name is obviously provisional.
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Text.JSON (showJSON, fromJSObject, fromJSString)
import Text.JSON.String (runGetJSON, readJSObject, readJSString)
import Text.JSON.Types (JSValue(..))
import Data.Maybe (fromJust)
import Control.Monad (join)

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
    runFunction $ fun x (showJSON items)
    where
    fun :: Element -> JSValue -> JSFunction ()
    fun = ffi "$(%1).autocomplete(\"option\", \"source\", %2);"

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

silence :: (Functor f) => f a -> f ()
silence = fmap (const ())

readJSObjectOptimistically :: String -> [(String, JSValue)]
readJSObjectOptimistically = fromJSObject
    . (\(Right (JSObject o)) -> o)
    . runGetJSON readJSObject

firstJSONEventResult :: EventData -> Maybe String
firstJSONEventResult ed = case ed of
    EventData []       -> Nothing
    EventData (mo : _) -> join $
        (fmap (\(JSString s) -> fromJSString s)
            . lookup "value" .  readJSObjectOptimistically)
        <$> mo

-- TODO: This should return a label-value pair.
autocompletechange :: Element -> Event (Maybe String)
autocompletechange = fmap firstJSONEventResult
    . domEvent "autocompletechange"

-- etc.

autocompleteselect :: Element -> Event (Maybe String)
autocompleteselect = fmap firstJSONEventResult
    . domEvent "autocompleteselect"
