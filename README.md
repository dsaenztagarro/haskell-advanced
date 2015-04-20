# Install

```
cabal install random
```

# Examples

```haskell
:load arrows/Hangman
total :: Num a => Circuit a a
total = accum' 0 (+)

-- We can run this circuit, like this:
-- *Main> runCircuit total [1,0,1,0,0,2]
-- [1,1,2,2,2,4]
-- *Main>

:load arrows/Kleisli
count'' "the" "fixture.txt"

:load arrows/Kleisli2
runKleisli (count "the") "fixture.txt"
runSF (arr (+1)) [1..5]
runSF (delay 0) [1..5]
runSF (arr id &&& delay 0) [1..5]
```

# Bibliography

- [Stephen's Arrow Tutorial](http://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)

- [The Continuation Monad](http://www.haskellforall.com/2012/12/the-continuation-monad.html)

### Papers

- Programming with Arrows (John Hughes). Department of Computer Science and 
  Engineering, Chalmers University of Technology, S-41296 Sweden.

