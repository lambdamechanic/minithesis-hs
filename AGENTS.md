# AGENTS

## Working Agreements
- Use GitHub CI with `haskell-actions/setup` v2 and cache Cabal with the latest `actions/cache` release.
- Build and test exclusively with Cabal; Stack must not be introduced.
- Before every commit, run linting, Ormolu formatting, `cabal test`, and `cabal check` locally.
  - `make format`
  - `hlint $(git ls-files '*.hs')`
  - `cabal test all --test-show-details=direct`
  - `cabal check`
- After each significant commit, push a suitably named branch and raise a PR.
- Branching policy: always ensure `main` is up to date before branching.
  - Run `git fetch origin --prune && git checkout main && git pull --ff-only`.
  - Then create your feature branch from `main` (e.g., `git checkout -b port/<topic>`).
  - This avoids unnecessary rebases later.
- The Haskell test suite must be written with Hspec while still providing runners for both Hspec and Tasty.
- Port the Python reference test suite incrementally, marking each Python test as `-- PORTED` once its Haskell counterpart exists.
- The `gh` CLI is available; use it for GitHub operations (e.g., opening PRs) when automation helps.

## Formatting
- Ormolu version: we use `ormolu-0.8.0.2` in CI. Install it locally once so results match:
  `cabal install ormolu-0.8.0.2`
- Run Ormolu in-place on all tracked Haskell files (explicit and CI-consistent):
  `cabal exec -- ormolu --mode inplace $(git ls-files '*.hs')`
- `make format` runs the same command.

### Red/Green Push Policy
- Always separate failing tests and the fix into two pushes on the same PR:
  - First push: add one or more failing test(s) that demonstrate the bug or missing behavior. Push this commit so CI reports the red build.
  - Second push: add the implementation/fix so the same PR goes green. Keep both commits in the PR history (do not squash away the failing test commit).
