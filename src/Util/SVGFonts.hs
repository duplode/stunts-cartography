{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module Util.SVGFonts where

import qualified Graphics.SVGFonts.Fonts as SVGFonts (bit)
import Graphics.SVGFonts.ReadFont (PreparedFont)
import Language.Haskell.TH.Syntax
import Instances.TH.Lift
import Data.Serialize
import Data.Either (fromRight)

-- A compile-time alternative to unsafePerformIO SVGFonts.bit
-- Note that restricting this to Double might limit the range of
-- compatible diagrams backends. We might eventually need variants for
-- different choices of n.
bit :: PreparedFont Double
bit = fromRight
    (error "Util.SVGFonts.Bit: deserialization failure")
    (decode $(runIO SVGFonts.bit >>= lift . encode @(PreparedFont Double)))
