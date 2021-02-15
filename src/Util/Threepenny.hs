module Util.Threepenny
    ( removeAttr
    , value_Text
    ) where

import Data.Text (Text)
import Graphics.UI.Threepenny.Core

removeAttr :: String -> UI Element -> UI Element
removeAttr name el = do
    x <- el
    runFunction $ ffi "$(%1).removeAttr(%2)" x name
    el

-- The same as the Threepenny definition, except for the type.
value_Text :: Attr Element Text
value_Text = mkReadWriteAttr get set
    where
    get   el = callFunction $ ffi "$(%1).val()" el
    set v el = runFunction  $ ffi "$(%1).val(%2)" el v
