## Database Setup

After installing Postgres, run:

```
createuser start9-companion-server --pwprompt --superuser
# Enter password start9-companion-server when prompted
createdb start9-companion-server
createdb start9-companion-server_test
```

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

### Development tools

`ghcid "-c=stack ghci --test"`

- Clone [HIE](https://github.com/haskell/haskell-ide-engine)
- Checkout latest reslease ie. `git checkout tags/1.3`
- Follow github instructions to install for specific GHC version ie. `stack ./install.hs hie`
- Install VSCode Haskell Language Server Extension

To create `hie.yaml` if it does not exist:
- gather executables by running `stack ide targets`
- see [here](https://github.com/haskell/haskell-ide-engine#project-configuration) for file setup details

## Tests

```
stack test --flag start9-companion-server:library-only --flag start9-companion-server:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

### Tests with HIE Setup
- install hspec-discover globally `cabal install hspec-discover` (requires cabal installation)
- Current [issue](https://github.com/haskell/haskell-ide-engine/issues/1564) open for error pertaining to obtaining flags for test files
	- recommended to setup hie.yaml
	- recommended to run `stack build --test --no-run-tests` *before* any test files are open and that test files compile without error 
	- helps to debug a specific file: `hie --debug test/Main.hs`

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.