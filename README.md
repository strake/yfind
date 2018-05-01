# yfind

Search for translating patterns (a.k.a. spaceships) in cellular automata.

Now only does hexagonal rules; should do square/full Moore rules in future.

Experimental/alpha quality

## Building

You need [git](https://git-scm.com/) and [stack](https://haskell-lang.org/get-started).

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
