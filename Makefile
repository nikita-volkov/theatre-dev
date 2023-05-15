.PHONY: test bench

build: clean format test master

format:
	for path in $$(git diff --staged --name-only -- '*.cabal') $$(git ls-files -om --exclude-standard -- '*.cabal'); do if test -f $$path; then cabal-fmt --no-tabular -c $$path 2> /dev/null || cabal-fmt --no-tabular -i $$path; fi; done
	for path in $$(git diff --staged --name-only -- '*.hs') $$(git ls-files -om --exclude-standard -- '*.hs'); do if test -f $$path; then ormolu -ic $$path; fi; done

test: 
	cabal build --builddir dist/test --disable-optimisation --run-tests --test-show-details always -j +RTS -A128m -n2m -N -RTS

master:
	cabal build --builddir dist/master --enable-tests --enable-benchmarks -j +RTS -A128m -n2m -N -RTS --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing -Wno-unused-do-bind"

docs:
	cabal haddock --enable-documentation --builddir dist/docs

clean:
	rm -rf dist
