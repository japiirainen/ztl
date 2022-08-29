## ztl

What is this?? That's a good question..


## Usage of server

Run the `server` parser through a file and show the output.

```sh
cabal run server -- parse <filename>
```

You can also run the test suite.

```sh
cabal test tasty
```

## Nix support

You can alternatively use nix for dev environment and for building the project.

Build:

```sh
nix build .
```

Run:

```sh
nix run .
```

Start Nix shell:

```sh
nix-shell
```

## Tips

- Run `nix flake update` to update all flake inputs.
- Run `./bin/hoogle` to start Hoogle with packages in your cabal file.
- Run `treefmt` in nix shell to autoformat the project. This uses treefmt, which uses ./treefmt.toml (where fourmolu and nixpkgs-fmt are specified).
- Run the application without installing: `nix run github:lattialle/server` (or `nix run .` from checkout)

## TODO for initial feature set

- [ ] TODO
