PATH=$PATH:~/.cabal/bin
export path
idris +RTS -K2048000000 -RTS --log 0 --build overmind.ipkg
