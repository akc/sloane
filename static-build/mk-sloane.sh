#!/bin/sh

cd /tmp/x

if cd sloane
then
    git pull
else
    git clone https://github.com/akc/sloane.git
    cd sloane
fi

ghc --make -O2 -optl-static sloane.hs
strip -s sloane
