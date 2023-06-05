build:
	stack build

run:
	stack run -- $(ARGS)

clean:
	stack clean
	rm -rf ./release

release:
	stack build --ghc-options -O2

install: release
	stack install
