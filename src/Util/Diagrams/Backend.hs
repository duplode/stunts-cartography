{-# LANGUAGE CPP #-}
module Util.Diagrams.Backend
    ( B
    , renderBE
    , OutputType(..)
    , defaultOutputType
    , alternativeOutputTypes
    , forkRender
    ) where

import qualified Data.List.NonEmpty as NonEmpty

import Util.Diagrams.Backend.Common (OutputType(..))
# if defined(CAIRO_BACKEND)
import Util.Diagrams.Backend.Cairo (B, renderBE, forkRender, outputTypes)
# elif defined(SVG_BACKEND)
import Util.Diagrams.Backend.SVG (B, renderBE, forkRender, outputTypes)
# elif defined(RASTERIFIC_BACKEND)
import Util.Diagrams.Backend.Rasterific (B, renderBE, forkRender, outputTypes)
# endif

defaultOutputType :: OutputType
defaultOutputType = NonEmpty.head outputTypes

alternativeOutputTypes :: [OutputType]
alternativeOutputTypes = NonEmpty.tail outputTypes
