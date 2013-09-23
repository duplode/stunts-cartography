module Util.Reactive.Threepenny where

import Control.Monad (void, when)

import Reactive.Threepenny
import Graphics.UI.Threepenny.Core (ReadWriteAttr, set')

reactimate :: Event (IO ()) -> IO ()
reactimate e = void $ register e id

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

-- Deprecated. There is no need at all to use this function to make getters.
newEventsTagged :: Ord tag => IO (tag -> Event a, (tag, a) -> IO ())
newEventsTagged = do
    (eTrigger, fireTrigger) <- newEvent
    let tagHandler (tag, _, fire) =
            void $ register (filterE ((== tag) . fst) eTrigger) (fire . snd)
    e <- newEventsNamed tagHandler
    return (e, fireTrigger)

sinkWhen :: Behavior Bool -> ReadWriteAttr x i o -> Behavior i -> IO x -> IO x
sinkWhen bp attr bi mx = do
    x <- mx
    i <- currentValue bi
    p <- currentValue bp
    when p $ set' attr i x
    onChange bi $ \i -> do
        p <- currentValue bp
        when p $ set' attr i x
    return x

{-
sinkWhen :: Behavior Bool -> ReadWriteAttr x i o -> Behavior i -> IO x -> IO x
sinkWhen bp attr bi mx = do
    x <- mx
    let sinkHandler i = do
            p <- currentValue bp
            when p $ set' attr i x
    currentValue bi >>= sinkHandler
    onChange bi sinkHandler
    return x
    -}