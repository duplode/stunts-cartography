Build notes for Windows as of 1.0.0.0. 
These will eventually be laid out in a neater way.

* Install msys2 (and update its packages, etc.)

* See https://github.com/haskell-gi/haskell-gi/wiki/Using-haskell-gi-in-Windows

  * Do the environment variable changes through the relevant Windows dialog

    * Though it's probably better to only add C:\msys64\mingw64\bin to the PATH
  
  * Use the MSYS shell to use pacman, and only for that
  
  * Install the GTK packages with pacman

* Install ghcup, selecting the installed msys2

* Switch to the desired GHC

* Build the program with cabal

* Use https://stackoverflow.com/a/50130668 to find the DLLs used by the exe

  * `ldd mygtkapp.exe | grep '\/mingw.*\.dll' -o | xargs -I{} cp "{}" .` on the 
    mingw64 shell will copy all libraries to the current directory

* Library licenses can be found in e.g. C:\msys64\mingw64\share\licenses
