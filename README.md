# yfind

Search for translating patterns (a.k.a. spaceships) in cellular automata.

## Building

You need [git](https://git-scm.com/), [stack](https://haskell-lang.org/get-started), and [z3](https://github.com/Z3Prover/z3).

```sh
git clone https://github.com/strake/yfind
chdir yfind
stack build
```

## Using

For example, to search the rule B2o45/S2o45 for spaceships of maximum size (8,8) and speed (1,1)/3:

```sh
stack exec -- yfind -rB2o45/S2o45 -s'(8,8)' -v'(1,1)/3'
```
