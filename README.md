# Minithesis (Haskell)

Minithesis is a deliberately small property-based testing library that is being ported from [the Python reference implementation](reference/README.md) to Haskell. The focus is on offering an approachable codebase for learning while still providing practical generators and shrinking.

## Setup

- Install recent GHC and Cabal builds (the maintainers use `ghcup` to manage both).
- Run `cabal update` once after cloning to prime the package index.
- Skim `PLAN.md` to understand which pieces of the Python implementation remain to be ported.

## Day-to-day Development

The repository is Cabal-first and ships a small Makefile helper:

- `cabal build`
- `make format` (runs `ormolu-0.7.7.0` over all tracked `.hs` files)
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
spec = do
  describe "Examples with Hspec interface" $ do
    MH.prop "sorting twice equals sorting once" $
      withTests 200 $ \tc -> do
        xs <- any tc $ lists (integers (-10) 10) (Just 0) (Just 20)
        sort (sort xs) `shouldBe` sort xs
```

## Reference Material

- `PLAN.md` tracks the incremental porting roadmap.
- `reference/` contains the authoritative Python implementation that we mirror feature-by-feature.

## Contributing

See `CONTRIBUTING.md` for collaboration guidelines.
