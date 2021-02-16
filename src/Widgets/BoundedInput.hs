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
    -- Appearance modifiers
    , formatBoundsCaption
    , setTextInputWidth
    -- Raw events
    , userValueChange
    -- Widget state manipulation
    , withRefresh
    ) where

import Text.Read (readMaybe)
import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Util.Threepenny (sinkWhen)
import Util.Threepenny.Flexbox

data BoundedInput a = BoundedInput
    { _itxValue :: Element
    , _strRange :: Element
    , _spnWrapper :: Element

    , _minimumValue :: a
    , _maximumValue :: a

    , _userValueChange :: Event a
    , _eRefresh        :: Event ()
    , _refresh         :: () -> IO () -- TODO: Is this the best type?
    }

userValueChange :: BoundedInput a -> Event a
userValueChange = _userValueChange

withRefresh :: BoundedInput a -> Event b -> UI (Event b)
withRefresh bi e = do
    onEvent e $ \_ -> liftIO $ _refresh bi ()
    return e

enforceBounds :: (Ord a) => BoundedInput a -> a -> a
enforceBounds bi = min (_maximumValue bi) . max (_minimumValue bi)

new :: (Ord a, Show a, Read a)
    => (a, a) -> UI (BoundedInput a)
new (_minimumValue, _maximumValue) = do

    _itxValue <- UI.input # set UI.type_ "text" #. "bounded-input-value"
        # set style [("width", "4em")]
    _strRange <- string
        ("(" ++ show _minimumValue ++ " - " ++ show _maximumValue ++ ") ")
        #. "bounded-input-caption"

    let _userValueChange = filterJust $ readMaybe <$> UI.valueChange _itxValue

    (_eRefresh, _refresh) <- liftIO newEvent

    _spnWrapper <- rowFlex #. "bounded-input" #+
        map element [ _strRange, _itxValue ]

    return BoundedInput {..}

plugModel :: (Ord a, Show a, Read a)
          => BoundedInput a -> Behavior a -> UI (Behavior a)
plugModel bi bModel = do

    let itx = _itxValue bi
        bValue = enforceBounds bi <$> bModel
    _defaultValue <- currentValue bValue

    bEditing <- stepper False $ and <$>
        unions [ True <$ UI.focus itx, False <$ UI.blur itx ]
    void $ element itx # sinkWhen (not <$> bEditing) value (show <$> bValue)

    onEvent (bValue <@ _eRefresh bi) $ (element itx #) . set value . show

    return bValue

simpleModel :: (Ord a, Show a, Read a)
           => a -> Event (a -> a)
           -> BoundedInput a -> UI (Behavior a)
simpleModel initialValue eSet bi = do
    let eValue = unionWith const eSet $ const <$> _userValueChange bi
    initialValue `accumB` eValue >>= plugModel bi

userModel :: (Ord a, Show a, Read a)
         => a -> BoundedInput a -> UI (Behavior a)
userModel = (`simpleModel` never)

formatBoundsCaption :: ((a, a) -> String)
                    -> UI (BoundedInput a) -> UI (BoundedInput a)
formatBoundsCaption fFormat biM = do
    bi <- biM
    _strRange <- element (_strRange bi)
        # set text (' ' : fFormat (_minimumValue bi, _maximumValue bi))
    return bi { _strRange }

setTextInputWidth :: Int -> UI (BoundedInput a) -> UI (BoundedInput a)
setTextInputWidth sz biM = do
    bi <- biM
    _itxValue <- element (_itxValue bi) # set style [("width", show sz ++ "em")]
    return bi { _itxValue }

instance Widget (BoundedInput a) where
    getElement = _spnWrapper

