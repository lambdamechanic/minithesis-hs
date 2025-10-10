CABAL ?= cabal
# Use the Cabal-installed ormolu explicitly for consistency with CI
ORMOLU ?= $(CABAL) exec -- ormolu
HLINT ?= hlint
HS_SOURCES := $(shell git ls-files -- '*.hs')

.PHONY: format lint test check validate

format:
	@if [ -n "$(HS_SOURCES)" ]; then \
		$(ORMOLU) --mode inplace $(HS_SOURCES); \
	else \
		echo "No Haskell sources to format."; \
	fi

lint:
	@if [ -n "$(HS_SOURCES)" ]; then \
		$(HLINT) $(HS_SOURCES); \
	else \
		echo "No Haskell sources to lint."; \
	fi

test:
	$(CABAL) test all --test-show-details=direct

check:
	$(CABAL) check minithesis
	$(CABAL) check minithesis-hspec
	$(CABAL) check minithesis-tasty
	$(CABAL) check minithesis-sydtest
	$(CABAL) check minithesis-generic

validate: format lint test check
