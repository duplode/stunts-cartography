module Types.Diagrams where

import Diagrams.Backend.Cairo (Cairo)

-- The default diagrams backend.
type BEDia = Cairo

-- Here there used to be a bunch of type synonyms for common diagram-like
-- types. They have been removed, and will only be reinstated if redefining
-- them for diagrams-1.3+ turns out to be useful.
