#!/bin/sh

cabal-dev configure --enable-tests && cabal-dev build && ./dist/build/runTests/runTests
