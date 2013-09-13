{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Widgets.BoundedInput
    ( BoundedInput
    -- Construction
    , new
    -- Model definition and setup
    , plugModel
    , simpleModel
    , userModel
    -- Default renderer
    , toElement
    -- Appearance modifiers
    , formatBoundsCaption
    , setTextInputSize
    -- Raw events
    , userValueChange
    -- Widget state manipulation
    , withRefresh
    ) where

import Text.Read (readMaybe)
import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Util.Reactive.Threepenny (sinkWhen)

data BoundedInput a = BoundedInput
    { _itxValue :: Element
    , _strRange :: Element

    , _minimumValue :: a
    , _maximumValue :: a

    , _userValueChange :: Event a
    , _eRefresh        :: Event ()
    , _refresh         :: () -> IO ()
    }

userValueChange :: BoundedInput a -> Event a
userValueChange = _userValueChange

withRefresh :: BoundedInput a -> Event b -> IO (Event b)
withRefresh bi e = do
    void $ register e $ \_ -> _refresh bi ()
    return e

enforceBounds :: (Ord a) => BoundedInput a -> a -> a
enforceBounds bi = min (_maximumValue bi) . max (_minimumValue bi)

new :: (Ord a, Show a, Read a)
    => (a, a) -> IO (BoundedInput a)
new (_minimumValue, _maximumValue) = do

    _itxValue <- UI.input # set UI.type_ "text" # set UI.size "5"
        #. "bounded-input-value"
    _strRange <- string
        (" (" ++ show _minimumValue ++ " - " ++ show _maximumValue ++ ")")
        #. "bounded-input-caption"

    let _userValueChange = filterJust $ readMaybe <$> UI.valueChange _itxValue

    (_eRefresh, _refresh) <- newEvent

    return BoundedInput {..}

plugModel :: (Ord a, Show a, Read a)
          => BoundedInput a -> Behavior a -> IO (Behavior a)
plugModel bi bModel = do

    let itx = _itxValue bi
        bValue = enforceBounds bi <$> bModel
    _defaultValue <- currentValue bValue

    let eBlur = UI.blur itx
    bEditing <- stepper False $ and <$>
        unions [ True <$ UI.domEvent "focus" itx, False <$ eBlur ]
    void $ element itx # sinkWhen (not <$> bEditing) value (show <$> bValue)

    let eSync = bValue <@ unionWith const eBlur (_eRefresh bi)
    void $ register eSync $ void . (element itx #) . set value . show

    return bValue

simpleModel :: (Ord a, Show a, Read a)
           => a -> Event (a -> a)
           -> BoundedInput a -> IO (Behavior a)
simpleModel initialValue eSet bi = do
    let eValue = unionWith const eSet $ const <$> _userValueChange bi
    initialValue `accumB` eValue >>= plugModel bi

userModel :: (Ord a, Show a, Read a)
         => a -> BoundedInput a -> IO (Behavior a)
userModel = (`simpleModel` never)

formatBoundsCaption :: ((a, a) -> String)
                    -> IO (BoundedInput a) -> IO (BoundedInput a)
formatBoundsCaption fFormat biM = do
    bi <- biM
    _strRange <- element (_strRange bi)
        # set text (' ' : fFormat (_minimumValue bi, _maximumValue bi))
    return bi { _strRange }

setTextInputSize :: Int -> IO (BoundedInput a) -> IO (BoundedInput a)
setTextInputSize sz biM = do
    bi <- biM
    _itxValue <- element (_itxValue bi) # set UI.size (show sz)
    return bi { _itxValue }

toElement :: BoundedInput a -> IO Element
toElement bi =
    UI.span #. "bounded-input" #+
        map element [ _itxValue bi, _strRange bi ]

