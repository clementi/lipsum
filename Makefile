build:
	stack build

clean:
	stack clean
	rm -rf ./release

release:
	stack build --ghc-options -O2
