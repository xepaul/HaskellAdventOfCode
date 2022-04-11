# Haskell AdventOf Code solutions

Solutions to Advent of Code problems 

![CI](https://github.com/xepaul/HaskellAdventOfCode/actions/workflows/haskell.yml/badge.svg)

# Tests
Run with 
```
cabal test all --test-show-details=direct
```
or
```
cabal install doctest --overwrite-policy=always && cabal build && cabal repl --build-depends=QuickCheck --build-depends=template-haskell --with-ghc=doctest
```