cabal2ghci
==============

This is the tool to automatically generate `.ghci` file and `.stylish-haskell.yaml` file from `.cabal`. It processes following things:

* Language Pragmas
* hs-src-dirs

Installation
-------------
```
$ cabal install cabal2ghci
```

Usage
------
```
$ cabal2ghci
$ cabal2ghci -c foo.cabal
$ cabal2ghci --nostylish
```
