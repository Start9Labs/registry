all:
	stack build --local-bin-path dist --copy-bins
profile:
	stack build --local-bin-path dist --copy-bins --profile
