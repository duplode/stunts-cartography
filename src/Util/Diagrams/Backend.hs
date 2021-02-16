{-# LANGUAGE CPP #-}
module Util.Diagrams.Backend
    ( B
    , renderBE
    , OutputType(..)
    , defaultOutputType
    , alternativeOutputTypes
    , widthConversionFactor
    , forkRender
    ) where

import qualified Data.List.NonEmpty as NonEmpty

import Util.Diagrams.Backend.Common (OutputType(..))
# if defined(CAIRO_BACKEND)
import Util.Diagrams.Backend.Cairo
# elif defined(SVG_BACKEND)
import Util.Diagrams.Backend.SVG
# elif defined(RASTERIFIC_BACKEND)
import Util.Diagrams.Backend.Rasterific
# endif
    (B, renderBE, forkRender, availableOutputTypes, widthConversionFactor)

defaultOutputType :: OutputType
defaultOutputType = NonEmpty.head availableOutputTypes

alternativeOutputTypes :: [OutputType]
alternativeOutputTypes = NonEmpty.tail availableOutputTypes
