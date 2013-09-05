module Util.Reactive.Threepenny where

import Control.Monad (void)

import Reactive.Threepenny

reactimate :: Event (IO ()) -> IO ()
reactimate e = void $ register e id

newEventsTagged :: Ord tag => IO (tag -> Event a, (tag, a) -> IO ())
newEventsTagged = do
    (eTrigger, fireTrigger) <- newEvent
    let tagHandler (tag, _, fire) =
            -- reactimate $ fire . snd <$> filterE ((== tag) . fst) eTrigger
            void $ register (filterE ((== tag) . fst) eTrigger) (fire . snd)
    e <- newEventsNamed tagHandler
    return (e, fireTrigger)

union :: Event a -> Event a -> Event a
union = unionWith const

unionLast :: Event a -> Event a -> Event a
unionLast = unionWith (flip const)

unionDot :: Event (a -> a) -> Event (a -> a) -> Event (a -> a)
unionDot = unionWith (.)

concatE :: [Event (a -> a)] ->  Event (a -> a)
concatE = foldr unionDot never

