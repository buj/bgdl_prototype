## How to compile

Source code is Haskell, GHC 8.8.1 suffices.
Requires the following libraries: `ilist`, `set-monad` (install through `cabal`)

## How to run

The program outputs all the "models", in some (currently undocumented,
and mostly a work-in-progress) semantics.

There are plenty of examples in the test folder. Try to find out what
it actually does! (The `==>` operator is obvious, `--o` less so, and
maybe there are some non-obvious interactions between them?)

```
ghc Main.hs -o main
./main < ./test/prop_logic.in
./main < ./test/override_implicit.in
./main < ./test/override_explicit.in
./main < ./test/safe.in
```
