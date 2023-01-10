{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Types.CartoM
    ( CartoT
    , CartoM
    , cartoT
    , runCartoT
    , mapCartoT
    ) where

import Control.Monad.Reader
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.State.Strict
import qualified Control.Monad.Trans.State.Strict as State
import Control.Monad.Trans.Writer.CPS (WriterT (..))
import qualified Control.Monad.Trans.Writer.CPS as Writer
import Control.Monad.Writer.Class
import Control.Monad.Error.Class
import Control.Monad.Identity
import Parameters

-- CartoT, the Cartography monad transformer.
-- type CartoT m = RWST RenderingParameters RenderingLog RenderingState m
newtype CartoT m a = CartoT { getCartoT
    :: ReaderT RenderingParameters
        (StateT RenderingState
            (WriterT RenderingLog m)) a}
    deriving (Functor, Applicative, Monad, MonadIO
        , MonadReader RenderingParameters, MonadState RenderingState)

-- TODO: Replace these with deriving once we can use mtl-2.3.1.
instance MonadTrans CartoT where
    lift = CartoT . lift . lift . lift

instance Monad m => MonadWriter RenderingLog (CartoT m) where
    tell x = CartoT . lift . lift $ Writer.tell x
    listen (CartoT m) = CartoT $
        Reader.mapReaderT (State.liftListen Writer.listen) m
    pass (CartoT m) = CartoT $
        Reader.mapReaderT (State.liftPass Writer.pass) m

instance MonadError e m => MonadError e (CartoT m) where
    throwError = lift . throwError
    catchError m h = CartoT $
        Reader.liftCatch (State.liftCatch (Writer.liftCatch catchError))
            (getCartoT m)
            (getCartoT . h)

type CartoM a = CartoT Identity a

type CartoTriple a = ((a, RenderingState), RenderingLog)

cartoT
    :: Functor m
    => (RenderingParameters -> RenderingState -> m (CartoTriple a))
    -> CartoT m a
cartoT f = CartoT . Reader.ReaderT $ \p ->
    State.StateT $ \s -> Writer.writerT (f p s)

runCartoT
    :: CartoT m a
    -> RenderingParameters -> RenderingState -> m (CartoTriple a)
runCartoT (CartoT m) p s = Writer.runWriterT $
    State.runStateT (Reader.runReaderT m p) s

mapCartoT
    :: (Functor m, Functor n)
    => (m (CartoTriple a) -> n (CartoTriple b))
    -> CartoT m a -> CartoT n b
mapCartoT f m = cartoT $ \p s -> f ((runCartoT m) p s)
