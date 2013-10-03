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

setter :: Event a -> Event (a -> a)
setter = fmap const

-- TODO: Update when the boolean is toggled.
sinkWhen :: Behavior Bool -> ReadWriteAttr x i o -> Behavior i -> UI x -> UI x
sinkWhen bp attr bi mx = do
    x <- mx
    window <- askWindow
    liftIOLater $ do
        i <- currentValue bi
        p <- currentValue bp
        runUI window $ when p $ set' attr i x
        onChange bi $ \i -> do
            p <- currentValue bp
            runUI window $ when p $ set' attr i x
    return x

