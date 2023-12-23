# linear-parallel - Parallel Computation Interfaces for Linear Haskell

This package provides linear pure parallelism utilities.

See [`linear-fft`](../linear-fft) for example usage.

## Design Notes

With the linear setting, pure combinator `runEval` can contain arbitrary pure expression that calls destructive operations under the hood.
So `runEval` always uses `unsafePerformIO`-like operations, **NOT** `Dupable` ones for safety. This could sacrifice efficiency in some cases.

## Contents

- [`Control.Parallel.Linear`](./src/Control/Parallel/Linear.hs): Linear variants of `par` and `seq` done right.
- [`Control.Parallel.Strategy.Linear`](./src/Control/Parallel/Strategy/Linear.hs): Linear variant of parallelism combinators from [`parallel`](https://hackage.haskell.org/package/parallel) package.
