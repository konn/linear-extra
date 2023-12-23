# linear-witness

This package provides the `Linearly` witness token, which enables uniform resource allocation as described in the "[Linear Constraints: the Problem with Scopes_](https://www.tweag.io/blog/2023-03-23-linear-constraints-linearly/)" by Arnaud Spiwack.
This is just a tentative workaround until [Linear Constraints](https://github.com/ghc-proposals/ghc-proposals/pull/621) proposal is implemented in GHC.

## Contents

- [`Linear.Token.Linearly`](./src/Linear/Token/Linearly.hs) provides `Linearly` constraints and related combinators.
- [`Linear.Token.Borrowing`](./src/Linear/Token/Borrowing.hs) provides a borrowing-related linear tokens, such as `R` and `W`.
- [`Data.**.Linear.Witness`](./src/Data) provides allocation primitives for containers provided in `linear-base`
