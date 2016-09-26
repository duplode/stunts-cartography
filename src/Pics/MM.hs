{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Pics.MM
    ( acura
    , acura'
    ) where

import Diagrams.Prelude
import Pics.Palette (sunroofCl, windshieldCl)
import Types.Diagrams (BEDia)

acura cl =
    (
        rect (1/10) (1/4) # fc sunroofCl # lwG 0
        <> roundedRect (1/4) (1/4) (1/20) # lwG 0.01 # fc windshieldCl)
    # translateX (-1/32)
    <>
    roundedRect' (1/2) (3/10)
        (with & radiusTR .~ 1/10 & radiusBR .~ 1/10)
    # lwG 0.01 # fc cl

-- TODO: Further abstract the base size.
acura' cl sz = acura cl # scale (2 * sz)

