# About
A program evaluating formulae in CPC and returning set of n-tuples, for which each formula is false as the output.

#Prerequisites
* [Haskell's GHC](https://www.haskell.org/ghc/)
* *optional* [nix](https://nixos.org/download.html)

# Compilation
Run `cabal build`.
In case of problems, use nix.
Open `nix-shell`, then run `cabal build`.


# Usage
The program is designed to work in a piped manner.

Although it works interactively, it's best to treat it like this:
* `cat {formulae.file} | cabal run`, where *formulae.file* is a text file with newline-separated formulas of CPC.
* `echo f1\nf2\n...\nfn | cabal run`, granted that every formula fi is placed in the new line

Example input:
```
a /\ b
a \/ (b /\ c)
~(a \/ b)
```
example output:
```
a /\ b
[a,b]
[0,0]
[1,0]
[0,1]
a \/ (b /\ c)
[a,b,c]
[0,0,0]
[0,1,0]
[0,0,1]
~(a \/ b)
[a,b]
[1,0]
[0,1]
```
