module Types.CartoM where

import Control.Monad.RWS
import Control.Monad.Identity
import Parameters

-- CartoT, the Cartography monad transformer.
type CartoT b m =
    RWST (RenderingParameters b) RenderingLog (RenderingState b) m

-- CartoM, the Cartography monad.
type CartoM b = CartoT b Identity

