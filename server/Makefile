build:
	stack build --pedantic --haddock --ghc-options="-O2 -threaded"

watch:
	ghcid --command "stack ghci" --run --reload settings.yaml \
		--restart stack.yaml --restart package.yaml --restart default-settings.yaml

docs: build
	stack haddock --open qbfc

run: build
	stack exec qbfc-server
