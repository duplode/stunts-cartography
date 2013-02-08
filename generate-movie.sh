#!/bin/sh
rm -f movie/*
ghc -O --make Animate.hs
time ./Animate -o movie/frame.png -w 960 --fpu 20
