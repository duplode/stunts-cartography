module CartoM where

import Control.Monad.RWS
import Control.Monad.Identity
import Parameters

-- CartoT, the Cartography monad transformer.
type CartoT m = RWST RenderingParameters String RenderingState m

-- CartoM, the Cartography monad.
type CartoM = CartoT Identity

