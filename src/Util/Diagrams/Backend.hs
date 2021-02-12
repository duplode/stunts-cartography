{-# LANGUAGE CPP #-}
module Util.Diagrams.Backend
    ( BEDia
    , renderBE
    , OutputType(PNG, SVG)
    ) where

import Diagrams.Prelude
# if defined(CAIRO_BACKEND)
import Util.Diagrams.Backend.Cairo (BEDia, renderBE)
# elif defined(SVG_BACKEND)
import Util.Diagrams.Backend.SVG (BEDia, renderBE)
# elif defined(RASTERIFIC_BACKEND)
import Util.Diagrams.Backend.Rasterific (BEDia, renderBE)
# endif

data OutputType
    = PNG
    | SVG
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
