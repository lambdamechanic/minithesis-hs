# AGENTS

## Working Agreements
- Use GitHub CI with `haskell-actions/setup` v2 and cache Cabal with the latest `actions/cache` release.
- Build and test exclusively with Cabal; Stack must not be introduced.
- Before every commit, run linting, Ormolu formatting, `cabal test`, and `cabal check` locally.
- After each significant commit, push a suitably named branch and raise a PR.
- The Haskell test suite must be written with Hspec while still providing runners for both Hspec and Tasty.
- Port the Python reference test suite incrementally, marking each Python test as `-- PORTED` once its Haskell counterpart exists.

## Formatting
- Ormolu version: we use `ormolu-0.7.7.0` in CI. Install it locally once so results match:
  `cabal install ormolu-0.7.7.0`
- Run Ormolu in-place on all tracked Haskell files (explicit and CI-consistent):
  `cabal exec -- ormolu --mode inplace $(git ls-files '*.hs')`
- `make format` runs the same command.
