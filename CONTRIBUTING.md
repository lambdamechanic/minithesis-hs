# Contributing

This project ports the Python Minithesis reference implementation to Haskell in small, reviewable steps. Please keep the Hspec suite authoritative and port the Python tests gradually—mark each Python test with `-- PORTED` once the equivalent Haskell coverage exists.

## Branching Workflow

1. `git fetch origin --prune && git checkout main && git pull --ff-only`
2. Create a feature branch from the updated `main`, e.g. `git checkout -b port/<topic>`.

## Local Checks Before Every Commit

- `make format`
- `hlint $(git ls-files '*.hs')`
- `cabal test all --test-show-details=direct`
- `cabal check`

Aim for red/green commits: push a failing regression test first, then follow up with the fix in a separate commit so CI documents the transition.

## CI Configuration

The GitHub workflow uses `haskell-actions/setup` v2 and caches Cabal with the latest `actions/cache` release. Example-only suites are guarded by the `ci-tests` Cabal flag to keep regular builds lean.

See `AGENTS.md` for the full set of working agreements and collaboration conventions.
