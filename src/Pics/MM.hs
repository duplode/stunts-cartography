{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Pics.MM
    ( acura
    , acuraMarker
    , xMarker
    , circleMarker
    , diamondMarker
    , dotMarker
    , arrowMarker
    ) where

import Data.Maybe (fromMaybe)

import Diagrams.Prelude

import Pics.Palette (sunroofCl, windshieldCl)

-- The Acura sprite, used for ghost car tiles.
acura cl =
    (
        rect (1/10) (1/4) # fc sunroofCl # lwG 0
        <> roundedRect (1/4) (1/4) (1/20) # lwG 0.01 # fc windshieldCl)
    # translateX (-1/32)
    <>
    roundedRect' (1/2) (3/10)
        (with & radiusTR .~ 1/10 & radiusBR .~ 1/10)
    # lwG 0.01 # fc cl

-- A variant of the Acura sprite used for car annotations.
acuraMarker cl sz _ = acura cl # scale (2 * sz)

xMarker cl sz mWid = (p2 (0, 0) ~~ p2 (1, 1) <> p2 (1, 0) ~~ p2 (0, 1))
    # strokePath
    # lwG (fromMaybe (min (1/8) (5 * sz / 16)) mWid) # lc cl
    # centerXY
    # scale sz

circleMarker cl sz mWid = circle (1/2)
    # strokePath
    # lwG (fromMaybe (min (1/8) (sz / 4)) mWid) # lc cl
    # centerXY
    # scale sz

diamondMarker cl sz _ = baseSquare # deform' 0.001 concav
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

dotMarker cl sz _ = circle (1/2)
    # strokePath
    # lwG 0 # fc cl
    # centerXY
    # scale sz

arrowMarker cl sz mWid = arrow' (with
        & arrowHead .~ arrowheadTriangle (7/20 @@ turn)
        & headLength .~ global (5 * wid)
    ) sz
    # fc cl # lc cl # lwG wid
    where
    wid = fromMaybe (min (1/20) (sz / 10)) mWid
