module Util.Threepenny
    ( removeAttr
    , value_Text
    , sinkWhen
    , checkboxUserModel
    ) where

import Control.Monad (when)
import Data.Text (Text)
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Reactive.Threepenny

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

sinkWhen :: Behavior Bool -> ReadWriteAttr x i o -> Behavior i -> UI x -> UI x
sinkWhen bp attr bi mx = do
    x <- mx
    window <- askWindow
    let bpi = pure (,) <*> bp <*> bi
    liftIOLater $ do
        (p, i) <- currentValue bpi
        runUI window $ when p $ set' attr i x
        onChange bpi $ \(p, i) -> runUI window $ when p $ set' attr i x
    return x

-- Basic wiring for a checkbox.
checkboxUserModel :: Bool -> Element -> UI (Behavior Bool)
checkboxUserModel initial checkbox = do
    element checkbox # set UI.checked initial
    initial `stepper` UI.checkedChange checkbox
