build:
	stack build

run:
	stack run

clean:
	stack clean
	rm -rf ./release

release:
	stack build --ghc-options -O2
