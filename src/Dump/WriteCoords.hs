module Dump.WriteCoords
    ( tShow
    , printAsRow
    , textFrom3D
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import Data.Tuple.Extra

tShow :: Show a => a -> Text
tShow = T.pack . show

printAsRow :: [a -> Text] -> a -> Text
printAsRow cells x = T.intercalate (T.pack "\t") (map ($ x) cells)

textFrom3D :: Show a => (a, a, a) -> Text
textFrom3D = printAsRow [tShow . fst3, tShow . snd3, tShow . thd3]
