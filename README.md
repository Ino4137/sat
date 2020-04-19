# Compilation
`cabal build` should work
in case of problems allow nix passthrough

# Usage
`cabal run` spits out an executable
only use lowercase variable names, starting at `a` ;^)

example:
```
a -> b
[a,b]
[1,0]
a -> a /\ d
[a,b,c,d]
[1,0,0,0]
[1,1,0,0]
[1,0,1,0]
[1,1,1,0]
```
in the second one it assumed that there are variables between a and d