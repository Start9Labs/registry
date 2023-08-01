all:
	stack build --local-bin-path dist --copy-bins
profile:
	stack build --local-bin-path dist --copy-bins --profile
cabal:
	cabal build
	# this step is specific for m1 devices ie. aarch64-osx
	sudo cp dist-newstyle/build/aarch64-osx/ghc-9.2.5/start9-registry-0.2.1/x/registry-publish/build/registry-publish/registry-publish /usr/local/bin/
