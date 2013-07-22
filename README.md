[![Build Status](https://travis-ci.org/duboisf/hexif.png?branch)](https://travis-ci.org/duboisf/hexif)

To build test:

cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
cabal test
