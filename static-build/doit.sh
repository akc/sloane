#!/bin/sh

docker build --rm -t sloane-musl .
docker run --name sloane sloane-musl
docker cp sloane:/sloane/sloane .
docker rm sloane
