# Haskell AdventOf Code solutions

Solutions to Advent of Code problems 

https://adventofcode.com


![CI](https://github.com/xepaul/HaskellAdventOfCode/actions/workflows/haskell.yml/badge.svg)

# Update hie 
```
gen-hie > hie.yaml
```
# Tests
Run with 
```
cabal test all --test-show-details=direct
```
or
```
cabal install doctest --overwrite-policy=always && cabal build && cabal repl --build-depends=QuickCheck --build-depends=template-haskell --with-ghc=doctest
```