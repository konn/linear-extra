# linear-memory - Missing utilities to manipulate off-heap allocated data in Linear Haskell

__CAUTION__: This package is under heavy construction. Perhaps use of `unsafeDupablePeformIO` can cause inconsistency (not investigated)!

Missing mutation combinators for linear resources allocated off-heap.

## Contents

- [`Foreign.Marshall.Pure.Extra`](./src/Foreign/Marshal/Pure/Extra.hs): missing combinators to manipulate and inspect `Box a`-values from `linear-base`, allocated off-heap, without releasing it.
- [`Data.Ref.Linear.ReferenceCount.ThreadUnsafe`](./src/Data/Ref/Linear/ReferenceCount/ThreadUnsafe.hs): Thread-*unsafe* reference-counted, strong and weak mutable references.
- [`Data.AtomicCounter.Linear`](./src/Data/AtomicCounter/Linear.hs): __Thread-safe__ atomic counter.
