{-# LANGUAGE CPP #-}
module Util.Diagrams.Backend
    ( BEDia
    , renderBE
    , OutputType(PNG, SVG)
    , forkRender
    ) where

import Diagrams.Prelude
# if defined(CAIRO_BACKEND)
import Util.Diagrams.Backend.Cairo (BEDia, renderBE, forkRender)
# elif defined(SVG_BACKEND)
import Util.Diagrams.Backend.SVG (BEDia, renderBE, forkRender)
# elif defined(RASTERIFIC_BACKEND)
import Util.Diagrams.Backend.Rasterific (BEDia, renderBE, forkRender)
# endif

data OutputType
    = PNG
    | SVG
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
