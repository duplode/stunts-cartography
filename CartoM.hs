module CartoM where

import Control.Monad.Reader
import Control.Monad.Identity
import qualified Parameters as Pm

-- CartoT, the Cartography monad transformer.
type CartoT m = ReaderT Pm.RenderingParameters m

-- CartoM, the Cartography monad.
type CartoM = CartoT Identity

