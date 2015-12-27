#!/bin/sh

cd /tmp/x

if cd sloane
then
    git pull
else
    git clone https://github.com/akc/sloane.git
    cd sloane
fi

cabal update
cabal install --only-dependencies

# This will give an error if pandoc isn't installed, but that is fine as
# we only call build to generate dist/build/autogen/cabal_macros.h. Is
# there some cleaner solution for this?
cabal build

ghc --make -O2 -optl-static -fforce-recomp \
    -optP-include -optPdist/build/autogen/cabal_macros.h sloane.hs

strip -s sloane
