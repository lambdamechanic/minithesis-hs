# AGENTS

## Working Agreements
- Use GitHub CI with `haskell-actions/setup` v2 and cache Cabal with the latest `actions/cache` release.
- Build and test exclusively with Cabal; Stack must not be introduced.
- Before every commit, run linting, Ormolu formatting, `cabal test`, and `cabal check` locally.
- After each significant commit, push a suitably named branch and raise a PR.
- The Haskell test suite must be written with Hspec while still providing runners for both Hspec and Tasty.
- Port the Python reference test suite incrementally, marking each Python test as `-- PORTED` once its Haskell counterpart exists.

### Red/Green Push Policy
- Always separate failing tests and the fix into two pushes on the same PR:
  - First push: add one or more failing test(s) that demonstrate the bug or missing behavior. Push this commit so CI reports the red build.
  - Second push: add the implementation/fix so the same PR goes green. Keep both commits in the PR history (do not squash away the failing test commit).
