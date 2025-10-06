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

Each adapter package ships a tiny regression suite that also serves as usage documentation. Run them directly from the project root:

- Hspec: `cabal test minithesis-hspec:minithesis-hspec-test`
- Tasty: `cabal test minithesis-tasty:minithesis-tasty-test`
- Sydtest: `cabal test minithesis-sydtest:minithesis-sydtest-test`

All suites honour the `MINITHESIS_MAX_EXAMPLES` environment variable at runtime. Set it on the command line to raise or lower the number of generated examples (the default is 100):

```bash
MINITHESIS_MAX_EXAMPLES=500 cabal test minithesis-hspec:minithesis-hspec-test
MINITHESIS_MAX_EXAMPLES=25 cabal test minithesis-tasty:minithesis-tasty-test
```

### Opting into the adapters

The core package stays free of framework dependencies. Pull in whichever integration package you need from Cabal/Stack:

```cabal
test-suite my-hspec-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  build-depends:
      base
    , minithesis
    , minithesis-hspec
    , hspec

test-suite my-tasty-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  build-depends:
      base
    , minithesis
    , minithesis-tasty
    , tasty
    , tasty-hunit

test-suite my-sydtest-tests
  type: exitcode-stdio-1.0
  hs-source-dirs: test
  main-is: Spec.hs
  build-depends:
      base
    , minithesis
    , minithesis-sydtest
    , sydtest
```

## Failure database

Minithesis always runs with a failure database. Counterexamples are persisted to `.minithesis-hs-db/` by default so the next execution replays the shrunk choice sequence before exploring new examples. Override the location by supplying your own `directoryDatabase` via `withRunOptions` if you want to store entries elsewhere.

### Writing properties

`Minithesis.Hspec.prop`, `Minithesis.Tasty.testProperty`, and `Minithesis.Sydtest.prop` accept either a raw `TestCase -> IO ()` function or the richer `Property` type. Use the helper `withTests` to override the number of examples from code while still respecting environment overrides for quick experimentation.

The Hspec example in particular is short enough to inline here for quick reference:

```haskell
module Examples.SydtestSpec (spec) where

import Data.List (sort)
import Minithesis
import qualified Minithesis.Sydtest as MS
import Test.Syd
import Prelude hiding (any)

spec :: Spec
spec =
  describe "sorting" $ do
    MS.prop "sorting twice equals sorting once" $
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

The same knob applies to the adapter suites shown above, so you can raise or lower exploration for Hspec, Tasty, or Sydtest runs uniformly.

## Reference material

- `PLAN.md` tracks remaining work in the port from the Python reference implementation.
- `reference/` contains the authoritative Python implementation (useful when you want to compare shrink traces).
- `CONTRIBUTING.md` documents collaboration guidelines and CI expectations.
