# Minithesis (Haskell)

Minithesis is a property-based testing toolkit with integrated shrinking and a test database.

## Why pick Minithesis?

- **Test failure database** – every interesting example can be cached on disk and replayed automatically the next time the suite runs. No more copying repro commands into bug trackers.
- **Integrated shrinking** – like Hedgehog but not QuickCheck, shrinking and generation are integrated, so explicit shrinkers are unnecessary, and it's impossible for shrinking to create a value that the generator could never have made.

## Usage at a glance

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
