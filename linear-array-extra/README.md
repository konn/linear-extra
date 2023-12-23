# linear-array-extra

Missing array and vector-related utilities in linear-base.

## Features

- [`Data.Array.Destination.Vector.Generic`](./src/Data/Array/Destination/Vector/Generic.hs): a destination-passing style array allocator for generic vectors.
- [`Data.Array.Destination.Unboxed`](./src/Data/Array/Destination/Unboxed.hs): a destination-passing style array allocator, specialised to unboxed vectors.
- [`Data.Array.Mutable.Linear.Storable`](./src/Data/Array/Mutable/Linear/Storable.hs): linear mutable arrays represented by raw pointer, maintained off-heap.
- [`Data.Array.Mutable.Linear.Storable.Borrowable`](./src/Data/Array/Mutable/Linear/Storable/Borrowable.hs): _Borrowable_ linear mutable arrays represented by raw pointer, maintained off-heap. It can be sliced and lent to other consumers using tokens.
  + See [`linear-fft`](../linear-fft) for example usage.
  + Known bug: `free`ing subslice of the array results in early free...
- [`Data.Array.Mutable.Linear.Primitive`](./src/Data/Array/Mutable/Linear/Primitive.hs): linear primitive mutable arrays (backed by `PrimArray#`).
- [`Data.Array.Mutable.Linear.Unboxed`](./src/Data/Array/Mutable/Linear/Unboxed.hs): linear unboxed mutable arrays (backed by unboxed `Vector` from `vector` package).
- [`Data.Array.Mutable.Linear.Class`](./src/Data/Array/Mutable/Linear/Class.hs): Abstraction over linear arrays.
- [`Data.Array.Polarized.*.Extra`](./src/Data/Array/Polarized): Pull- and push-arrays, which can read and write from generic vectors.
- [`Data.Unrestricted.Linear.Orphans.Vector`](./src/Data/Unrestricted/Linear/Orphans/Vector.hs): orphan `Consumable`, `Dupable`, and `Movable` instances for vectors.
- [`Data.Vector.Mutable.Linear.*`](./src/Data/Vector/Mutable/Linear): Growable linear mutable vectors backed by generic linear arrays.
