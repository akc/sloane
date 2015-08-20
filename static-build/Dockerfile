FROM nilcons/ghc-musl:stackage
MAINTAINER Anders Claesson

RUN cabal update
RUN cabal install sloane --only-dependencies

COPY mk-sloane.sh /usr/local/bin/

CMD /usr/local/bin/mk-sloane.sh
