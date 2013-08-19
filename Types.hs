{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Types where

import Diagrams.Prelude
import Diagrams.Backend.Cairo (Cairo)
import qualified Diagrams.TwoD.Text as TwoDT (Text)
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import qualified Parameters as Pm

-- CartoT, the Cartography monad transformer.
type CartoT m = ReaderT Pm.RenderingParameters m

-- CartoM, the Cartography monad.
type CartoM = CartoT Identity

-- The default diagrams backend.
type BEDia = Cairo

-- Some types commonly found when creating parts of tiles in Pics and elsewhere.
-- Included mostly to be used when doing diagnostics. To avoid the need for way
-- too many language extensions, we avoid using them in normal operation.
type PreDia t = (HasStyle t, TrailLike t, Transformable t, V t ~ R2) => t

type PreDiaAl t = (Alignable t) => PreDia t

type QDia b m = (Monoid m, Semigroup m, Backend b R2) => QDiagram b R2 m

type QDiaPaTx b m = ( Monoid m, Semigroup m
                    , Backend b R2, Renderable (Path R2) b, Renderable TwoDT.Text b)
                  => QDiagram b R2 m

type Dia b = (Backend b R2) => Diagram b R2

type DiaPaTx b = (Backend b R2, Renderable (Path R2) b, Renderable TwoDT.Text b)
               => Diagram b R2
