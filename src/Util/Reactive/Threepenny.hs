module Util.Reactive.Threepenny where

import Control.Monad (void, when)

import Reactive.Threepenny
import Graphics.UI.Threepenny.Core
    (ReadWriteAttr, set', UI, runUI, askWindow, liftIOLater)

union :: Event a -> Event a -> Event a
union = unionWith const

unionLast :: Event a -> Event a -> Event a
unionLast = unionWith (flip const)

unionDot :: Event (a -> a) -> Event (a -> a) -> Event (a -> a)
unionDot = unionWith (.)

concatE :: [Event (a -> a)] ->  Event (a -> a)
concatE = foldr unionDot never
