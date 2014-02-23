#!/bin/sh
mkdir -p movie
rm -f movie/*.png
ghc -O -i./src --make Animate.hs
# Remember that the default frame rate is 30fps.
time ./Animate -o movie/frame.png -w 960
