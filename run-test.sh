#!/bin/sh
ghc -i./src -O --make Main.hs
time ./Main -o test.svg -w 960
firefox test.svg

