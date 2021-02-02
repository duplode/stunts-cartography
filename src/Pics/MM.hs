{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Pics.MM
    ( acura
    , acura'
    , xMarker
    , circleMarker
    , diamondMarker
    ) where

import Diagrams.Prelude
import Pics.Palette (sunroofCl, windshieldCl)
import Types.Diagrams (BEDia)

-- The Acura sprite, used for ghost car tiles and as a default for
-- sprite annotations.
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

xMarker cl sz = (p2 (0, 0) ~~ p2 (1, 1) <> p2 (1, 0) ~~ p2 (0, 1))
    # strokePath
    # lwG (1/8) # lc cl
    # centerXY
    # scale sz

circleMarker cl sz = circle (1/2)
    # strokePath
    # lwG (1/8) # lc cl
    # centerXY
    # scale sz

diamondMarker cl sz = baseSquare # deform' 0.001 concav
    # rotate (45 @@ deg)
    # strokePath
    # lwG 0 # fc cl
    # scaleX (3/4)
    # scale sz

    where

    baseSquare :: Path V2 Double
    baseSquare = square 1 # centerXY

    perturb :: Double -> Double -> Double
    perturb w z = z - signum z * (1/12) * cos (pi * w)

    concav :: Deformation V2 V2 Double
    concav = Deformation $ \p ->
        perturb (p ^. _y) (p ^. _x) ^& perturb (p ^. _x) (p ^. _y)
