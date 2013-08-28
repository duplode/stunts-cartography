{-# LANGUAGE ScopedTypeVariables #-}
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
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event, newEvent, filterJust)
import qualified Graphics.UI.Threepenny.Core as Reg
    (Event, newEvent, newEventsTagged)

import Reactive.Banana
import Reactive.Banana.Threepenny

import Text.Read (readMaybe)
import Control.Monad (void)

type TagGet = String

data BoundedInput a = BoundedInput
    { _itxValue :: Element
    , _strRange :: Element

    , _defaultValue :: a
    , _minimumValue :: a
    , _maximumValue :: a

    , _setValueEvent :: Reg.Event a
    , _setValue :: a -> IO ()

    , _valueChangedEvent :: Reg.Event a
    , _valueChanged :: a -> IO ()

    , _requestValueEvent :: Reg.Event TagGet
    , _requestValue :: TagGet -> IO ()
    , _getValueEvent :: TagGet -> Reg.Event a
    , _getValue :: (TagGet, a) -> IO ()

    , _refreshEvent :: Reg.Event ()
    , _refresh :: () -> IO ()
    }

setValue :: BoundedInput a -> a -> IO ()
setValue = _setValue

valueChangedEvent :: BoundedInput a -> Reg.Event a
valueChangedEvent = _valueChangedEvent

requestValue :: BoundedInput a -> TagGet -> IO ()
requestValue = _requestValue

getValueEvent :: BoundedInput a -> TagGet -> Reg.Event a
getValueEvent = _getValueEvent

refresh :: BoundedInput a -> IO ()
refresh bi = _refresh bi ()

new :: (Ord a, Show a, Read a)
    => (a, a) -> a -> IO (BoundedInput a)
new (_minimumValue, _maximumValue) _defaultValue = do

    _itxValue <- UI.input # set UI.type_ "text" # set UI.size "5"
        #. "bounded-input-value"
    _strRange <- string
        (" (" ++ show _minimumValue ++ " - " ++ show _maximumValue ++ ")")
        #. "bounded-input-caption"

    (_setValueEvent, _setValue) <- Reg.newEvent

    (_valueChangedEvent, _valueChanged) <- Reg.newEvent

    (_requestValueEvent, _requestValue) <- Reg.newEvent
    (_getValueEvent, _getValue) <- Reg.newEventsTagged

    (_refreshEvent, _refresh) <- Reg.newEvent

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            eUserInput <- eventValue _itxValue

            let (eInvalidInput, eUserValue) = split $
                    maybe (Left ()) Right . readMaybe <$> eUserInput

            eSetValue <- fromAddHandler (register _setValueEvent)

            eBlur <- fromAddHandler (register $ UI.blur _itxValue)
            eRequestValue <- fromAddHandler (register _requestValueEvent)
            eRefresh <- fromAddHandler (register _refreshEvent)

            let (eBoundSetValue, eInBoundsSetValue) = split $
                    enforceBounds <$> eSetValue

                (eBoundUserValue, eInBoundsUserValue) = split $
                    enforceBounds <$> eUserValue

                eBoundValue = eBoundUserValue `union` eBoundSetValue
                eInBoundsValue = eInBoundsUserValue `union` eInBoundsSetValue

                -- Left, in this passage, means "no correction is necessary".
                eSync = eBlur `union` eRefresh

                bCorrectValue = Left () `stepper` union
                    (Right <$> eBoundValue) (Left () <$ eInBoundsValue)
                (_, eCorrectOnSync) = split $ bCorrectValue <@ eSync

                bUndoInput = Left () `stepper` union
                    (Right <$> (bValue <@ eInvalidInput)) (Left () <$ eUserValue)
                (_, eUndoOnSync) = split $ bUndoInput <@ eSync
                (_, eUndoOnRequest) = split $ bUndoInput <@ eRequestValue

                -- These complications are needed because we cannot rely on the
                -- value of bValue as-is - eRequestValue may trigger changes to
                -- it in case there is a correction to be made.
                (eDontCorrectOnRequest, eCorrectOnRequest) = split $
                    (addTagToCorrection <$> bCorrectValue) <@> eRequestValue
                eValidOnRequest = ((<$) <$> bValue) <@> eDontCorrectOnRequest
                eGetValue = eCorrectOnRequest `union` eValidOnRequest

                eCorrectedValue = eCorrectOnSync
                    `union` (snd <$> eCorrectOnRequest)

                eValue = eInBoundsValue `union` eCorrectedValue

                bValue = _defaultValue `stepper` eValue

                eUndoValue = eUndoOnSync `union` eUndoOnRequest

                eSetTextValue = eUndoValue
                    `union` eCorrectedValue `union` eInBoundsSetValue

            reactimate $ _valueChanged <$> eValue

            reactimate $ _getValue <$> eGetValue

            reactimate $
                (void . (element _itxValue #) . set UI.value . show)
                    <$> eSetTextValue

    compile networkDescription >>= actuate

    _setValue _defaultValue

    return BoundedInput {..}

    where
    enforceBounds x
        | x < _minimumValue = Left _minimumValue
        | x > _maximumValue = Left _maximumValue
        | otherwise         = Right x

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

