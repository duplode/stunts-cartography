{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Types.Diagrams where

import qualified Diagrams.Backend.Cairo as Cairo
import qualified Diagrams.Backend.SVG as SVG
import qualified Diagrams.Backend.Rasterific as Rasterific
import Diagrams.Prelude
import qualified Diagrams.TwoD.Text as TwoD

-- The default diagrams backend.
--type BEDia = Cairo

-- Here there used to be a bunch of type synonyms for common diagram-like
-- types. They have been removed, and will only be reinstated if redefining
-- them for diagrams-1.3+ turns out to be useful.
data OutputType = PNG | SVG
    deriving (Eq)

class ( V b ~ V2, N b ~ Double
      , Renderable (TwoD.Text Double) b
      , Renderable (Path V2 Double) b
      ) => BeDi b where
    renderBeDi :: FilePath -> SizeSpec V2 Double
               -> QDiagram b V2 Double Any -> IO ()

instance BeDi Cairo.Cairo where
    renderBeDi = Cairo.renderCairo

instance BeDi SVG.SVG where
    renderBeDi = SVG.renderSVG

instance BeDi Rasterific.Rasterific where
    renderBeDi = Rasterific.renderRasterific
