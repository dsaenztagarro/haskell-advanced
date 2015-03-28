# Install

```
cabal install random
```

# Hangman

```haskell
total :: Num a => Circuit a a
total = accum' 0 (+)

-- We can run this circuit, like this:
-- *Main> runCircuit total [1,0,1,0,0,2]
-- [1,1,2,2,2,4]
-- *Main>
```
