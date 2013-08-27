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
    , receiveValueEvent
    ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (Event, newEvent, filterJust)
import qualified Graphics.UI.Threepenny.Core as Reg (Event, newEvent)

import Reactive.Banana
import Reactive.Banana.Threepenny

import Text.Read (readMaybe)
import Data.Maybe (isNothing)

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

    , _requestValueEvent :: Reg.Event ()
    , _requestValue :: () -> IO ()
    , _receiveValueEvent :: Reg.Event a
    , _receiveValue :: a -> IO ()
    }

setValue :: BoundedInput a -> a -> IO ()
setValue = _setValue

valueChangedEvent :: BoundedInput a -> Reg.Event a
valueChangedEvent = _valueChangedEvent

requestValue :: BoundedInput a -> IO ()
requestValue bi = (_requestValue bi) ()

receiveValueEvent :: BoundedInput a -> Reg.Event a
receiveValueEvent = _receiveValueEvent

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
    (_receiveValueEvent, _receiveValue) <- Reg.newEvent

    let networkDescription :: forall t. Frameworks t => Moment t ()
        networkDescription = do

            eUserInput <- ((readMaybe <$>))
                <$> eventValue _itxValue

            let eUserValue = filterJust eUserInput
                eInvalidInput = filterE isNothing $ eUserInput

            eSetValue <- pure union <*> pure eUserValue
                <*> fromAddHandler (register _setValueEvent)

            let (eBoundValue, eInBoundsValue) = split $ enforceBounds <$> eSetValue

            eBlur <- fromAddHandler (register $ UI.blur _itxValue)
            eRequestValue <- fromAddHandler (register _requestValueEvent)

            let eUndoValue = bValue <@ eInvalidInput

                bCorrectValue = Nothing `stepper` unions
                    [ Just <$> eUndoValue
                    , Just <$> eBoundValue
                    , Nothing <$ eInBoundsValue
                    ]
                eCorrectOnBlur = filterJust $ bCorrectValue <@ eBlur
                eValue = eInBoundsValue `union` eCorrectOnBlur

                -- These complications are needed because we cannot rely on the
                -- value of bValue as is if eRequestValue may trigger changes to
                -- it in case there is a correction.
                eCorrectOnRequest = filterJust $ bCorrectValue <@ eRequestValue
                eValidOnRequest = bValue <@
                    (filterE isNothing $ bCorrectValue <@ eRequestValue)
                eGetValue = eCorrectOnRequest `union` eValidOnRequest

                bValue = _defaultValue `stepper` (eValue `union` eGetValue)

            return _itxValue # sink UI.value (show <$> bValue)

            -- TODO: We might not want to trigger _valueChanged via eUndoValue .
            reactimate $ _valueChanged <$> eValue

            reactimate $ _receiveValue <$> eGetValue

    compile networkDescription >>= actuate

    _setValue _defaultValue

    return BoundedInput {..}

    where
    enforceBounds x
        | x < _minimumValue = Left _minimumValue
        | x > _maximumValue = Left _maximumValue
        | otherwise         = Right x

{-
-- We would like to write the element modifiers in terms of something like:
modifyElement :: (BoundedInput a -> Element)
              -> (IO (Element a) -> IO (Element a))
              -> IO (BoundedInput a) -> IO (BoundedInput a)
-- But record updates are not first class, and so that would require something
-- like lens.
-}

formatBoundsCaption :: (Show a, Read a) => ((a, a) -> String)
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
    UI.div #. "bounded-input" #+
        map element [ _itxValue bi, _strRange bi ]


