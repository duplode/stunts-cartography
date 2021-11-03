module Dump.WriteCoords
    ( tShow
    , cglom
    , textFrom3D
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

-- Having a little fun with the pretty printer, in the spirit of
-- https://www.haskellforall.com/2021/10/co-applicative-programming-style.html

tShow :: Show a => Op Text a
tShow = Op (T.pack . show)

sep :: Op Text a
sep = Op (const (T.pack "\t"))

cglom :: Op Text a -> Op Text b -> Op Text (a, b)
cglom p q = divided p (sep <> q)

infixr 5 `cglom`

textFrom3D :: Show a => Op Text (a, a, a)
textFrom3D = adapt >$< tShow `cglom` tShow `cglom` tShow
    where
    adapt (x, y, z) = (x, (y, z))
