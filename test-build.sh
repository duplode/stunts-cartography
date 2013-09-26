#!/bin/sh
ghc -i./src --make -hidir _hsbuild -outputdir _hsbuild src/Viewer.hs -o ./Viewer -O -rtsopts -fforce-recomp -prof -auto-all -caf-all
#ghc -i./src --make -hidir _hsbuild -outputdir _hsbuild src/Viewer.hs -o ./Viewer
