# hcomp
A L compiler written in [Haskell](https://haskell.org).

L is a very simple language defined in [this](http://citeseer.ist.psu.edu/plotkin81structural.html) [Gordon Plotkin's](https://en.wikipedia.org/wiki/Gordon_Plotkin) paper.

### Compiling hcomp's tests

To compile the hcomp's main (with all the tests) you'll need the [GHC](https://www.haskell.org/downloads) compiler

$ ghc -dynamic Main.hs -o hcomp

### Executing hcomp's tests

To execute hcomp's tests, after compiling it run the executable genarated

$ ./hcomp

And see the magic!