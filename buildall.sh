#!/bin/sh

cabal clean && cabal configure --enable-tests --disable-library-profiling && cabal build && cabal test
