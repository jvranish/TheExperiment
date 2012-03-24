#!/bin/sh

cabal-dev configure --enable-tests --sandbox=sandbox
cabal-dev build
cabal-dev test
