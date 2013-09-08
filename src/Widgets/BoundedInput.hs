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
    , setValue
    , valueChangedEvent
    , getValue
    , refresh
    , resetValue
    -- Miscellanea
    , listenAsPair
    ) where

import Text.Read (readMaybe)
import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Util.Reactive.Threepenny (concatE, union, reactimate)

data BoundedInput a = BoundedInput
    { _itxValue :: Element
    , _strRange :: Element

    , _minimumValue :: a
    , _maximumValue :: a
    , _defaultValue :: a

    , _setValueEvent :: Event a
    , _setValue :: a -> IO ()

    , _valueChangedEvent :: Event a
    , _valueChanged :: a -> IO ()

    , _getValue :: Event () -> IO (Event a)

    , _refreshEvent :: Event ()
    , _refresh :: () -> IO ()

    , _resetValueEvent :: Event ()
    , _resetValue :: () -> IO ()
    }

setValue :: BoundedInput a -> a -> IO ()
setValue = _setValue

valueChangedEvent :: BoundedInput a -> Event a
valueChangedEvent = _valueChangedEvent

getValue :: BoundedInput a -> Event () -> IO (Event a)
getValue = _getValue

refresh :: BoundedInput a -> IO ()
refresh bi = _refresh bi ()

resetValue :: BoundedInput a -> IO ()
resetValue bi = _resetValue bi ()

new :: (Ord a, Show a, Read a)
    => (a, a) -> a -> IO (BoundedInput a)
new (_minimumValue, _maximumValue) defaultValue = mdo

    _itxValue <- UI.input # set UI.type_ "text" # set UI.size "5"
        #. "bounded-input-value"
    _strRange <- string
        (" (" ++ show _minimumValue ++ " - " ++ show _maximumValue ++ ")")
        #. "bounded-input-caption"

    let _defaultValue = quietlyEnforceBounds defaultValue

    (_setValueEvent, _setValue) <- newEvent

    (_valueChangedEvent, _valueChanged) <- newEvent

    (_refreshEvent, _refresh) <- newEvent

    (_resetValueEvent, _resetValue) <- newEvent

    (eRequest, acknowledgeRequest) <- newEvent

    (eSyncOnSet, acknowledgeProgSet) <- newEvent

    let eUserInput = UI.valueChange _itxValue

        eUserValue = filterJust $ readMaybe <$> eUserInput

        eSetValue = quietlyEnforceBounds <$> _setValueEvent

        eResetValue = _defaultValue <$ _resetValueEvent

        eBlur = UI.blur _itxValue

        eInBoundsUserValue = quietlyEnforceBounds <$> eUserValue

        eProgSetValue = eSetValue `union` eResetValue

        eInBoundsValue = eInBoundsUserValue `union` eProgSetValue

        eSync = () <$ unions
            [ eBlur, eSyncOnSet, eRequest, _refreshEvent ]

    bValue <- _defaultValue `stepper` eInBoundsValue

    let _getValue e = do
            reactimate $ acknowledgeRequest <$> e
            return $ bValue <@ e

    reactimate $ _valueChanged <$> eInBoundsValue

    reactimate $ acknowledgeProgSet <$> (() <$ eProgSetValue)

    reactimate $
        void . (element _itxValue #) . set UI.value . show
            <$> (bValue <@ eSync)

    -- Completing the initialization.

    _setValue _defaultValue

    return BoundedInput {..}

    where
    quietlyEnforceBounds = max _minimumValue . min _maximumValue

    addTagToCorrection corr tag = case corr of
        Left () -> Left (tag, ())
        Right x -> Right (tag, x)

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
        [ (\z -> \(_, y) -> (z, y)) <$> _valueChangedEvent fstBI
        , (\w -> \(x, _) -> (x, w)) <$> _valueChangedEvent sndBI
        ]

