module Util.Diagrams.Backend.Common
    ( OutputType(..)
    ) where

data OutputType
    = PNG
    | SVG
    deriving (Eq, Ord, Show, Read, Enum, Bounded)
