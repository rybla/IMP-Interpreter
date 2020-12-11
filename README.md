# IMP-Interpreter

This is a small Haskell implementation of the IMP language, as specified in the [K Framework](https://kframework.org) tutorial here: [imp.md](https://github.com/kframework/k/blob/master/k-distribution/tutorial/1_k/2_imp/lesson_5/imp.md).
This was written as a programming exercise for applying to an internship at [Runtime Verification](https://www.runtimeverification.com),



## Building

This is a [Haskell stack](https://docs.haskellstack.org/en/stable/README/) project, which should easily build via

```sh
stack build
```

There may be some minor annoyances with managing the lens library dependency, which I had to add in the `extra-deps` of the `stack.yaml`.
Troubleshooting:
- make sure to have the [lens](http://hackage.haskell.org/package/lens) library installed with stack (e.g. `stack install lens`)
- try building again
- if this doesn't work, try commenting out `extra-deps` in `stack.yaml` and then building



## Running

In `src/app/Main.hs`, there is a `program` variable which you may modify to encode any IMP program you like. There is currently no parser yet written for this implementation, but there are many convenient abbreviations (viz `src/IMP/Grammar.hs`).
Run `src/app/Main.hs` via

```sh
stack run
```

The output will look something like

```
[IMP-Interpreter]
[program]
int n, sum ; n = 100 ; sum = 0 ; while (! n <= 0) { sum = sum + n ; n = n + -1 ; }
[output]
Success (fromList [(n,0),(sum,5050)])
```

where the `[program]` section prints the `program` variable in `src/app/Main.hs`, and the `[output]` section prints the resulting state after interpreting the program.



## Testing

In `src/test/Spec.hs`, there is a test suite of a few programs and their expected outputs (inspired by the examples given [here](https://github.com/kframework/k/tree/master/k-distribution/tutorial/1_k/2_imp/lesson_5/tests)).
Run the test suite via

```sh
stack test
```

If all the tests are successful, you'll get an output that looks something like:

```
✓ sum
✓ collatz
✓ primes
```

If there are errors (i.e. differences between the expected output and the actual output) then you'll get an error message like:

```
✓ sum
✗ collatz
  [!] m: expected 2 but found 3
✓ primes
```

where `m` is name of the variable in `collatze` that after interpreting `collatze` was expected to have value `2` but actually had value `3`.



## Organization

This package contains one module, `IMP`, with two submodules:
- `IMP.Grammar`: datatypes for the grammar of the IMP language, along with convenient abbreviations and smart constructors
- `IMP.Semantics`: implementation of interpretation for the IMP language, based around the transformed monad `Interpretation` that keeps track of the interpretation state and allows for failure.
