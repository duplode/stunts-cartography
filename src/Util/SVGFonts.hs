{-# LANGUAGE TemplateHaskell, TypeApplications #-}
module Util.SVGFonts where

import qualified Graphics.SVGFonts.Fonts as SVGFonts (bit)
import Graphics.SVGFonts.ReadFont (PreparedFont)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Serialize
import Data.FileEmbed (bsToExp)
import Data.Either (fromRight)

-- An alternative to unsafePerformIO SVGFonts.bit
-- Note that restricting this to Double might limit the range of
-- compatible diagrams backends.
bit :: PreparedFont Double
bit = fromRight
    (error "Util.SVGFonts.Bit: deserialization failure")
    (decode $(runIO SVGFonts.bit >>= bsToExp . encode @(PreparedFont Double)))
