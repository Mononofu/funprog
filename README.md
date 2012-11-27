Installation
============

First, you need to install QuickCheck from cabal.

If you don't have cabal, get it from your repositories:

```sudo aptitude install cabal-install```

Then, update your cache and install QuickCheck and HUnit:

```
cabal update
cabal install QuickCheck
cabal install HUnit
```


Usage
=====

Now, make sure that your code exports all functions (put `module Aufgabe4 where` at the top), then run the test script you want:

```
runhaskell Aufgabe4_test.hs
```

(in this case Aufgabe4.lhs needs to somewhere on the search path, easiest way is to put it into the same directory)


Contributions
=============

Everybody is welcome to contribute, just fork, edit and make a pull request!
