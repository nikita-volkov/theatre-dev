.PHONY: test bench

build: clean format test master

format:
	for path in $$(ls -d *.cabal); do cabal-fmt --no-tabular -c $$path 2> /dev/null || cabal-fmt --no-tabular -i $$path; done
	ormolu -ci $$(find . -name "*.hs" -not -path "./*.stack-work/*" -not -path "./dist/*" -not -path "./dist-newstyle/*" -not -path "./.git/*")

test: 
	cabal build --builddir dist/test --disable-optimisation --run-tests --test-show-details always -j +RTS -A128m -n2m -N -RTS

master:
	cabal build --builddir dist/master --enable-tests --enable-benchmarks -j +RTS -A128m -n2m -N -RTS --ghc-options="-Werror -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -Wredundant-constraints -Wunused-packages -Wno-name-shadowing -Wno-unused-do-bind"

docs:
	cabal haddock --enable-documentation --builddir dist/docs

clean:
	rm -rf dist
