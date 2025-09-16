# PLAN

## Maintenance Note
Keep this document up to date as a living record of porting progress from the Python reference to the Haskell implementation.

## Stage Overview
- ✅ Understand the Python reference implementation and requirements.
- ✅ Set up documentation for agents and establish workflow tracking.
- ✅ Initialize the Cabal project, add tooling (ormolu, linting), and configure CI with caching.
- ⏳ Port library features and tests from `reference/`, migrating each Python test to Hspec/Tasty and marking it `-- PORTED` once complete.

## Next Actions
- Remember to run `make format` (Ormolu) before committing to avoid CI failures.
- Map Python modules/tests to planned Haskell modules and capture design decisions.
- Implement the foundational testing state (e.g., `run_test`, `TestingState`) in Haskell.
- Port generator-driven tests (starting with `test_finds_small_list`) once the runtime pieces exist.
- Capture learnings in PLAN.md after each milestone and update CI/tooling if needs emerge.


## Ported Python Tests
- ✅ test_errors_when_using_frozen
- ✅ test_errors_on_too_large_choice
