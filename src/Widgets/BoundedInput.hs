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
    , requestValue
    , getValueEvent
    , refresh
    , resetValue
    -- Miscellanea
    , listenAsPair
    ) where

import Text.Read (readMaybe)
import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Util.Reactive.Threepenny

type TagGet = String

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

    , _requestValueEvent :: Event TagGet
    , _requestValue :: TagGet -> IO ()
    , _getValueEvent :: TagGet -> Event a
    , _getValue :: (TagGet, a) -> IO ()

    , _refreshEvent :: Event ()
    , _refresh :: () -> IO ()

    , _resetValueEvent :: Event ()
    , _resetValue :: () -> IO ()
    }

setValue :: BoundedInput a -> a -> IO ()
setValue = _setValue

valueChangedEvent :: BoundedInput a -> Event a
valueChangedEvent = _valueChangedEvent

requestValue :: BoundedInput a -> TagGet -> IO ()
requestValue = _requestValue

getValueEvent :: BoundedInput a -> TagGet -> Event a
getValueEvent = _getValueEvent

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

    (_requestValueEvent, _requestValue) <- newEvent
    (_getValueEvent, _getValue) <- newEventsTagged

    (_refreshEvent, _refresh) <- newEvent

    (_resetValueEvent, _resetValue) <- newEvent

    let eUserInput = UI.valueChange _itxValue

        (eInvalidInput, eUserValue) = split $
            maybe (Left ()) Right . readMaybe <$> eUserInput

        eSetValue = quietlyEnforceBounds <$> _setValueEvent

        eResetValue = _defaultValue <$ _resetValueEvent

    let eBlur = UI.blur _itxValue

    let (eBoundUserValue, eInBoundsUserValue) = split $
            enforceBounds <$> eUserValue

        -- Note that programatically set values are silently constrained
        -- to the bounds.
        eProgramaticallySetValue = eSetValue `union` eResetValue

        eInBoundsValue = eInBoundsUserValue
            `union` eProgramaticallySetValue

        -- Left, in this passage, means "no correction is necessary".
        eSync = eBlur `union` _refreshEvent

    bCorrectValue <- Left () `stepper` union
        (Right <$> eBoundUserValue) (Left () <$ eInBoundsValue)
    let (_, eCorrectOnSync) = split $ bCorrectValue <@ eSync

    bUndoInput <- Left () `stepper` union
        (Right <$> bValue <@ eInvalidInput) (Left () <$ eUserValue)
    let (_, eUndoOnSync) = split $ bUndoInput <@ eSync
        (_, eUndoOnRequest) = split $ bUndoInput <@ _requestValueEvent

        -- These complications are needed because we cannot rely on the value
        -- of bValue as-is - _requestValueEvent may trigger changes to it in
        -- case there is a correction to be made.
        (eDontCorrectOnRequest, eCorrectOnRequest) = split $
            addTagToCorrection <$> bCorrectValue <@> _requestValueEvent
        eValidOnRequest = ((<$) <$> bValue) <@> eDontCorrectOnRequest
        eGetValue = eCorrectOnRequest `union` eValidOnRequest

        eCorrectedValue = eCorrectOnSync
            `union` (snd <$> eCorrectOnRequest)

        eValue = eInBoundsValue `union` eCorrectedValue

    bValue <- _defaultValue `stepper` eValue

    let eUndoValue = eUndoOnSync `union` eUndoOnRequest

        eSetTextValue = eUndoValue `union` eCorrectedValue
            `union` eProgramaticallySetValue

    reactimate $ _valueChanged <$> eValue

    reactimate $ _getValue <$> eGetValue

    reactimate $
        void . (element _itxValue #) . set UI.value . show
            <$> eSetTextValue

    -- Completing the initialization.

    _setValue _defaultValue

    return BoundedInput {..}

    where
    enforceBounds x
        | x < _minimumValue = Left _minimumValue
        | x > _maximumValue = Left _maximumValue
        | otherwise         = Right x

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

