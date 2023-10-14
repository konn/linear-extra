{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-name-shadowing -funbox-strict-fields #-}

{- |
Linear analogue of "Control.Parallel.Strategy" from @parallel@ package.
Almost all codes are literal translation of the original code.

TODO: Linear-analogue of NFData.
-}
module Control.Parallel.Strategy.Linear (
  -- * The strategy type
  Strategy,

  -- * Application of strategies
  using,
  usingIO,
  withStrategy,
  withStrategyIO,

  -- * Composition of strategies
  dot,

  -- * Basic strategies
  r0,
  rseq,
  -- TODO: rdeepseq once linear version of NFData is formulated
  rpar,
  rparWith,

  -- * Strategies for traversable data types
  evalTraversable,
  parTraversable,

  -- * Strategic function application
  ($|),
  ($||),
  (.|),
  (.||),

  -- * For Strategy programmers
  Eval (),
  runEval,
  runEvalIO,
) where

import qualified Control.Functor.Linear as C
import qualified Data.Functor.Linear as D
import GHC.Exts
import qualified GHC.Exts as GHC
import GHC.IO (evaluate)
import Prelude.Linear
import qualified System.IO.Linear as LIO
import qualified Unsafe.Linear as Unsafe

type Strategy a = a %1 -> Eval a

infixr 9 `dot` -- same as (.)

infixl 0 `using`

infixl 0 `usingIO`

{- |
Evaluate a value using the given 'Strategy'.

> x `using` s = runEval (s x)
-}
using :: a %1 -> Strategy a %1 -> a
x `using` strat = runEval (strat x)

{- |
evaluate a value using the given 'Strategy'.  This is simply
'using' with the arguments reversed.
-}
withStrategy :: Strategy c %1 -> c %1 -> c
withStrategy = flip using

{- |
Evaluate a value using the given 'Strategy' inside the 'IO' monad.  See
also 'runEvalIO'.

> x `usingIO` s = runEvalIO (s x)
-}
usingIO :: a %1 -> Strategy a %1 -> LIO.IO a
x `usingIO` strat = runEvalIO (strat x)

{- |
Evaluate a value using the given 'Strategy' inside the 'IO' monad.  This
is simply 'usingIO' with the arguments reversed.
-}
withStrategyIO :: Strategy a %1 -> a %1 -> LIO.IO a
withStrategyIO = flip usingIO

{- | Compose two strategies sequentially.
This is the analogue to function composition on strategies.

For any strategies @strat1@, @strat2@, and @strat3@,

> (strat1 `dot` strat2) `dot` strat3 == strat1 `dot` (strat2 `dot` strat3)
> strat1 `dot` strat1 = strat1
> strat1 `dot` r0 == strat1

> strat2 `dot` strat1 == strat2 . withStrategy strat1
-}
dot :: Strategy a %1 -> Strategy a %1 -> Strategy a
{-# INLINE dot #-}
strat2 `dot` strat1 = strat2 . runEval . strat1

{- | 'r0' performs *no* evaluation.

> r0 == evalSeq Control.Seq.r0
-}
r0 :: Strategy a
r0 = C.pure

-- | 'rseq' evaluates its argument to weak head normal form.
rseq :: Strategy a
{-# NOINLINE [1] rseq #-}
rseq x = Eval (LIO.fromSystemIO (Unsafe.toLinear evaluate x))

-- | 'rpar' sparks its argument (for evaluation in parallel).
rpar :: Strategy a
{-# INLINE rpar #-}
rpar x = Eval $ LIO.IO $ Unsafe.toLinear2 GHC.spark# x

{- |
Perform a computation in parallel using a strategy.

@
rparWith strat x
@

will spark @strat x@. Note that @rparWith strat@ is /not/ the
same as @rpar `dot` strat@. Specifically, @rpar `dot` strat@
always sparks a computation to reduce the result of the
strategic computation to WHNF, while @rparWith strat@ need
not.

> rparWith r0 = r0
> rparWith rpar = rpar
> rparWith rseq = rpar

@rparWith rpar x@ creates a spark that immediately creates another
spark to evaluate @x@. We consider this equivalent to @rpar@ because
there isn't any real additional parallelism. However, it is always
less efficient because there's a bit of extra work to create the
first (useless) spark. Similarly, @rparWith r0@ creates a spark
that does precisely nothing. No real parallelism is added, but there
is a bit of extra work to do nothing.
-}
rparWith :: Strategy a %1 -> Strategy a
rparWith strat = parEval . strat

{- | Evaluate the elements of a traversable data structure
according to the given strategy.
-}
evalTraversable :: (D.Traversable t) => Strategy a -> Strategy (t a)
evalTraversable = D.traverse
{-# INLINE evalTraversable #-}

-- | Like 'evalTraversable' but evaluates all elements in parallel.
parTraversable :: (D.Traversable t) => Strategy a -> Strategy (t a)
parTraversable strat = evalTraversable (rparWith strat)
{-# INLINE parTraversable #-}

{- | Sequential function application. The argument is evaluated using
  the given strategy before it is given to the function.
-}
($|) :: (a %1 -> b) %1 -> Strategy a %1 -> a %1 -> b
f $| s = \x -> withStrategy s x & \ !z -> f z

{- | Parallel function application. The argument is evaluated using
the given strategy, in parallel with the function application.
-}
($||) :: (a %1 -> b) %1 -> Strategy a %1 -> a %1 -> b
{- HLINT ignore $|| -}
f $|| s = \x ->
  withStrategy s x & Unsafe.toLinear \z ->
    case par# z of
      _ -> f z

{- | Sequential function composition. The result of
the second function is evaluated using the given strategy,
and then given to the first function.
-}
(.|) :: (b %1 -> c) %1 -> Strategy b %1 -> (a %1 -> b) -> (a %1 -> c)
{- HLINT ignore .| -}
(.|) f s g = \x ->
  withStrategy s (g x) & \ !z -> f z

{- | Parallel function composition. The result of the second
function is evaluated using the given strategy,
in parallel with the application of the first function.
-}
(.||) :: (b %1 -> c) %1 -> Strategy b %1 -> (a %1 -> b) %1 -> (a %1 -> c)
{- HLINT ignore .|| -}
(.||) f s g = \x ->
  withStrategy s (g x) & Unsafe.toLinear \z ->
    case par# z of
      _ -> f z

newtype Eval a = Eval {_unEval :: LIO.IO a}
  deriving newtype (D.Functor, C.Functor, D.Applicative, C.Applicative, C.Monad)

runEval :: Eval a %1 -> a
{-# ANN runEval "HLint: ignore" #-}
runEval = Unsafe.toLinear \(Eval (LIO.IO k)) ->
  case runRW# (\s -> k s) of
    (# _, a #) -> a

runEvalIO :: Eval a %1 -> LIO.IO a
{-# ANN runEvalIO "HLint: ignore" #-}
runEvalIO = \(Eval a) -> a

{- | 'parEval' sparks the computation of its argument for evaluation in
parallel. Unlike @'rpar' . 'runEval'@, 'parEval'

 * does not exit the `Eval` monad

 * does not have a built-in `rseq`, so for example @'parEval' ('r0' x)@
   behaves as you might expect (it creates a spark that does no
   evaluation).

It is related to 'rparWith' by the following equality:

> parEval . strat = rparWith strat
-}
parEval :: Eval a %1 -> Eval a
-- The intermediate `Lift` box is necessary, in order to avoid a built-in
-- `rseq` in `parEval`. In particular, we want @parEval . r0 = r0@, not
-- @parEval . r0 = rpar@.
parEval m = C.do
  rpar (runEval (Lift C.<$> m)) C.<&> \case
    Lift x -> x

{- HLINT ignore Lift "Use newtype instead of data" -}
data Lift a = Lift a
