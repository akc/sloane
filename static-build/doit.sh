#!/bin/sh

docker build --rm -t sloane-musl .
docker run -v /tmp/x:/tmp/x -it --rm sloane-musl
cp /tmp/x/sloane/sloane .
