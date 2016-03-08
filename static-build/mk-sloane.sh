#!/bin/sh

if cd sloane 2>/dev/null
then
    git fetch
else
    git clone https://github.com/akc/sloane.git
    cd sloane
fi
git reset --hard origin/master

echo "constraints: http-conduit ==2.1.7" >cabal.config

cabal update
cabal install --only-dependencies

# This will give an error if pandoc isn't installed, but that is fine as
# we only call build to generate dist/build/autogen/cabal_macros.h. Is
# there some cleaner solution for this?
cabal configure
cabal build

ghc --make -O2 -optl-static -fforce-recomp \
    -optP-include -optPdist/build/autogen/cabal_macros.h sloane.hs

strip -s sloane
