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

Integration with [haskell-mode](https://github.com/haskell/haskell-mode)
-------------------------------------------------------------------------
```elisp
(add-hook 'haskell-mode-hook 'cabal2ghci-haskell-hook)
(defun my-before-save-hook ()
  (ignore-errors (call-process "cabal2ghci")))

(defun cabal2ghci-haskell-hook ()
  (add-hook 'before-save-hook 'my-before-save-hook)
```
