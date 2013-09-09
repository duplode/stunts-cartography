{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Widgets.BoundedInput
    ( BoundedInput
    -- Pseudo-constructors
    , new
    -- Default renderer
    , toElement
    -- Appearance modifiers
    , formatBoundsCaption
    , setTextInputSize
    -- Event interface
    , resetter
    , valueChange
    , progValueChange
    , anyValueChange
    , getValue
    , refresh
    -- Miscellanea
    , listenAsPair
    ) where

import Text.Read (readMaybe)
import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Util.Reactive.Threepenny (concatE, union, reactimate, setter)

data BoundedInput a = BoundedInput
    { _itxValue :: Element
    , _strRange :: Element

    , _minimumValue :: a
    , _maximumValue :: a
    , _defaultValue :: a

    , _valueChange     :: Event a
    , _progValueChange :: Event a
    , _anyValueChange  :: Event a
    , _getValue        :: Event () -> IO (Event a)
    , _refresh         :: () -> IO ()
    }

-- This is a little clumsy.
resetter :: BoundedInput a -> Event () -> Event (a -> a)
resetter bi = setter . (_defaultValue bi <$)

valueChange :: BoundedInput a -> Event a
valueChange = _valueChange

progValueChange :: BoundedInput a -> Event a
progValueChange = _progValueChange

anyValueChange :: BoundedInput a -> Event a
anyValueChange = _anyValueChange

getValue :: BoundedInput a -> Event () -> IO (Event a)
getValue = _getValue

refresh :: BoundedInput a -> IO ()
refresh bi = _refresh bi ()

new :: (Ord a, Show a, Read a)
    => (a, a) -> (a, Event (a -> a)) -> IO (BoundedInput a)
new (_minimumValue, _maximumValue) (defaultValue, eSet) = mdo

    _itxValue <- UI.input # set UI.type_ "text" # set UI.size "5"
        #. "bounded-input-value"
    _strRange <- string
        (" (" ++ show _minimumValue ++ " - " ++ show _maximumValue ++ ")")
        #. "bounded-input-caption"

    let enforceBounds = min _maximumValue . max _minimumValue
        _defaultValue = enforceBounds defaultValue

    (eRefresh, _refresh) <- newEvent

    (eRequest, acknowledgeRequest) <- newEvent

    (eSyncOnSet, acknowledgeProgSet) <- newEvent

    let eUserInput = UI.valueChange _itxValue

        eUserValue = enforceBounds <$> (filterJust $ readMaybe <$> eUserInput)

        eSetValue = enforceBounds <$> (flip ($) <$> bValue <@> eSet)

        eValue = eSetValue `union` eUserValue

        eBlur = UI.blur _itxValue

        eSync = () <$ unions
            [ eBlur, eSyncOnSet, eRequest, eRefresh ]

    bValue <- _defaultValue `stepper` eValue

    let _getValue e = do
            reactimate $ acknowledgeRequest <$> e
            return $ bValue <@ e

        -- Does this make the event fire *before* the value has actually
        -- changed?
        _valueChange = eUserValue
        _progValueChange = eSetValue
        _anyValueChange = eValue

    reactimate $ acknowledgeProgSet <$> (() <$ eSetValue)

    reactimate $
        void . (element _itxValue #) . set UI.value . show
            <$> (bValue <@ eSync)

    -- Completing the initialization.

    _refresh ()

    return BoundedInput {..}


formatBoundsCaption :: ((a, a) -> String)
                    -> IO (BoundedInput a) -> IO (BoundedInput a)
formatBoundsCaption fFormat mBI = do
    bi <- mBI
    _strRange <- element (_strRange bi)
        # set text (' ' : fFormat (_minimumValue bi, _maximumValue bi))
    return bi { _strRange }

setTextInputSize :: Int -> IO (BoundedInput a) -> IO (BoundedInput a)
setTextInputSize sz mBI = do
    bi <- mBI
    _itxValue <- element (_itxValue bi) # set UI.size (show sz)
    return bi { _itxValue }

toElement :: BoundedInput a -> IO Element
toElement bi =
    UI.span #. "bounded-input" #+
        map element [ _itxValue bi, _strRange bi ]

-- This approach might be extended to make a full compound widget.
listenAsPair :: BoundedInput a -> BoundedInput a -> IO (Event (a, a))
listenAsPair fstBI sndBI =
    (_defaultValue fstBI, _defaultValue sndBI) `accumE` concatE
        [ (\z -> \(_, y) -> (z, y)) <$> _anyValueChange fstBI
        , (\w -> \(x, _) -> (x, w)) <$> _anyValueChange sndBI
        ]

