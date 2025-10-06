# Minithesis (Haskell)

Minithesis is a property-based testing toolkit with integrated shrinking and a test database.

## Why pick Minithesis?

- **Test failure database** – every interesting example can be cached on disk and replayed automatically the next time the suite runs. No more copying repro commands into bug trackers.
- **Integrated shrinking** – like Hedgehog but not QuickCheck, shrinking and generation are integrated, so explicit shrinkers are unnecessary, and it's impossible for shrinking to create a value that the generator could never have made.

## Usage at a glance

## Day-to-day Development

The repository is Cabal-first and ships a small Makefile helper:

- `cabal build`
- `make format` (runs `ormolu-0.8.0.2` over all tracked `.hs` files)
- `hlint $(git ls-files '*.hs')`
- `cabal test all --test-show-details=direct`
- `cabal check`

Re-run the list above before every commit to stay in sync with CI expectations.

## Running the Examples

Example suites live under `examples/hspec` and `examples/tasty`. They are disabled by default and gated behind the `ci-tests` Cabal flag so that downstream users do not pay for them accidentally. Enable the flag when you want to execute them locally:

- Hspec: `cabal test -fci-tests examples-hspec`
- Tasty: `cabal test -fci-tests examples-tasty`

Both examples consult the `MINITHESIS_MAX_EXAMPLES` environment variable at runtime. Set it on the command line to raise or lower the number of generated examples (the default is 100):

```bash
MINITHESIS_MAX_EXAMPLES=500 cabal test -fci-tests examples-hspec
MINITHESIS_MAX_EXAMPLES=25 cabal test -fci-tests examples-tasty
```

### Opting into the adapters

The core library stays free of framework dependencies. Opt into the adapters explicitly from your package description:

```cabal
test-suite my-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  build-depends:
      base
    , minithesis
    , minithesis:hspec
    , hspec

test-suite tasty-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  build-depends:
      base
    , minithesis
    , minithesis:tasty
    , tasty
    , tasty-hunit
```

### Writing properties

`Minithesis.Hspec.prop` and `Minithesis.Tasty.testProperty` accept either a raw `TestCase -> IO ()` function or the richer `Property` type. Use the helper `withTests` to override the number of examples from code while retaining environment overrides for quick experimentation.

The Hspec example in particular is short enough to inline here for quick reference:
=======
## Day-to-day Development

The repository is Cabal-first and ships a small Makefile helper:

- `cabal build`
- `make format` (runs `ormolu-0.8.0.2` over all tracked `.hs` files)
- `hlint $(git ls-files '*.hs')`
- `cabal test all --test-show-details=direct`
- `cabal check`

Re-run the list above before every commit to stay in sync with CI expectations.

## Running the Examples

Example suites live under `examples/hspec` and `examples/tasty`. They are disabled by default and gated behind the `ci-tests` Cabal flag so that downstream users do not pay for them accidentally. Enable the flag when you want to execute them locally:

- Hspec: `cabal test -fci-tests examples-hspec`
- Tasty: `cabal test -fci-tests examples-tasty`

Both examples consult the `MINITHESIS_MAX_EXAMPLES` environment variable at runtime. Set it on the command line to raise or lower the number of generated examples (the default is 100):

```bash
MINITHESIS_MAX_EXAMPLES=500 cabal test -fci-tests examples-hspec
MINITHESIS_MAX_EXAMPLES=25 cabal test -fci-tests examples-tasty
```

## Failure database

Minithesis always runs with a failure database. Counterexamples are persisted to `.minithesis-hs-db/` by default so the next execution replays the shrunk choice sequence before exploring new examples. Override the location by supplying your own `directoryDatabase` via `withRunOptions` if you want to store entries elsewhere.

### Writing properties

`Minithesis.Hspec.prop` and `Minithesis.Tasty.testProperty` accept either a raw `TestCase -> IO ()` function or the richer `Property` type. Use the helper `withTests` to override the number of examples from code while retaining environment overrides for quick experimentation.

The Hspec example in particular is short enough to inline here for quick reference:

```haskell
module Examples.HspecSpec (spec) where

import Data.List (sort)
import Minithesis
import qualified Minithesis.Hspec as MH
import Test.Hspec
import Prelude hiding (any)

spec :: Spec
spec =
  describe "sorting" $ do
    MH.prop "sorting twice equals sorting once" $
      withTests 200 $ \tc -> do
        xs <- any tc $ lists (integers (-10) 10) (Just 0) (Just 20)
        sort (sort xs) `shouldBe` sort xs
```

- `withTests` tunes exploration while still respecting `MINITHESIS_MAX_EXAMPLES`.
- `any`, `lists`, `integers`, and the other strategies mirror Hypothesis names.
- When a failure occurs, Minithesis shrinks it, prints the exact choices, and records it in the optional database so it replays automatically next run.

### Raising or lowering exploration

Set `MINITHESIS_MAX_EXAMPLES` to control the global example budget (default 100):

```bash
MINITHESIS_MAX_EXAMPLES=500 cabal test minithesis-test --test-show-details=direct
```

The same knob applies to the example suites under `examples/hspec` and `examples/tasty`. They are disabled by default; enable them with the `ci-tests` flag when you want to exercise them locally:

```bash
cabal test -fci-tests examples-hspec
cabal test -fci-tests examples-tasty
```

## Reference material

- `PLAN.md` tracks remaining work in the port from the Python reference implementation.
- `reference/` contains the authoritative Python implementation (useful when you want to compare shrink traces).
- `CONTRIBUTING.md` documents collaboration guidelines and CI expectations.
