# Extra Utilities for Linear Haskell, missing in linear-base

Missing utilities for Linear Haskell, for array, witness token, memory, parallelism, and streaming.

- [`linear-witness`](./linear-witness/README.md): Linear Witness Tokens. Tentative workaround until we have [Linear Constraints](https://github.com/ghc-proposals/ghc-proposals/pull/621) in GHC.
- [`linear-array-extra`](./linear-array-extra/README.md): Missing arrays, such as borrowable arrays, unboxed/storable/primitive arrays/vectors and more.
- [`linear-memory`](./linear-memory/README.md): Off-heap allocation utilities.
- [`linear-parallel`](./linear-parallel/README.md): Parallelism combinators for pure linear expressions with side-effects.
- [`linear-fft`](./linear-fft/README.md): Demonstration implementation of pure, in-place and parallel Fast Fourier Transformation, using `linear-parallel` and `linear-array-extra`.
