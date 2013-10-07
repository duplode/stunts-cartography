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

