# Start9 Registry/Publish

This codebase is the reference implementation of the marketplace protocol. These steps assume you are running Linux (or macOS).

## Getting Started

### Installing Haskell

- Go get [GHCup](https://www.haskell.org/ghcup/)
-   select default options
-   restart your session (terminal or OS)
- run `ghcup tui`
- select the latest `Stack` and install with `i`

### Build the Registry

- `apt install libpq-dev` (on macOS - `brew install libpq`)
- run `apt install postgresql` (on macOS - `brew install postgres`)
- clone and cd into the registry 
```
git clone git@github.com:Start9Labs/registry.git
cd registry
```
- run `make`

### Set up registry-publish tool

- run `apt install libgmp-dev zlib1g-dev libtinfo-dev libpq-dev` (on macOS `brew install libmpd zlib-ng libtiff`)
- run `stack install` (recommended: include the installation path in your $PATH after running this command)
- update your shell to include the installation path of the copied executables from `stack install`. i.e. `nano ~./zshrc` add `export PATH=$PATH:/your/path/here` to zshrc; save and exit nano. Run `source ~/.zshrc`
- run `registry-publish init --bash` (or --zsh / --fish depending on your preferred shell)
- run `registry-publish reg add -l <URL> -n <NAME> -u <USER> -p <PASS>` (include https:// in your URL)
- take the hash that is emitted by this command and submit it to the registry owner

### Setting up a registry dev environment

- set up a local database in postgresql
- set the PG_DATABASE environment variable to that database
- set PG_USER to the owner of that database
- set PG_PASSWORD to the password for that user
- set SSL_AUTO to false
- set RESOURCES_PATH to an empty directory you wish to use as your package repository
- install `start-sdk`
- set STATIC_BIN to the path that contains `start-sdk`

## APIs

### EOS

This API is exclusive to Start9, we do not publish this as part of the standard Marketplace protocol because Start9 does
not intend for other organizations to host prebuilt OS binaries per the Non Commercial License

### Package

This API is a public API that can be implemented by any marketplace. This is how information about available packages
and their relationships is queried by EOS.

### Admin

This API is how we upload new packages to the marketplace. It requires authentication and is not intended to be implemented
by other marketplaces
