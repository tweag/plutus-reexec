# Plutus Script Re-Executor

A tool that follows a Cardano node in a "leashed" (resource efficient) connection mode, scans for scripts hashes provided by user and executes customly specified PlutusCore scripts instead of the original ones. That allows to run a debugging PlutusCore script with traces in the context of a ledger state provided by the node to ease up the development process of the scripts.

## How to Run

### Get a binary:

With [flake-enabled](https://nixos.wiki/wiki/Flakes) Nix, run Plutus Script Re-Executor directly `nix run github:tweag/plutus-reexec`.
Please note, that [our binary cache](http://plutus-script-reexecutor.cachix.org) provides only `aarch64-macos` packages, meaning you might need to build a lot of packages yourself.

You can install also install the binary in your profile with `nix profile install github:tweag/plutus-reexec`. The `plutus-script-reexecutor` command is then in your `$PATH` and is available anywhere.

Or you can build it directly, see [development](#Development) section.

### Configuration

The re-executor takes a configuration file with scripts to substitute: `plutus-script-reexecutor --script-yaml scripts.yaml`. The example file can be generated with `plutus-script-reexecutor generate-scripts-example` command.

Please note that `plutus-script-reexecutor` uses `CARDANO_NODE_SOCKET_PATH` and `CARDANO_NODE_NETWORK_ID` environment variables by default, which can be overriden with cli flags `--node-socket-file PATH (--mainnet | --testnet TESTNET_MAGIC)`.

### Running

After starting the `plutus-script-reexecutor` you can look for events with `websocat ws://localhost:8080/events-ws` or `curl -v http://localhost:8080/events | jq`. 

## Documentation

See the [docs](./docs) directory for detailed documentation.

## Development 

To build and run the project manually:

```bash
nix develop
cabal build
cabal run plutus-script-reexecutor
```

We use `process-compose` to run the local cluster with cardano-node, testing scripts and re-executor. Use `./dev-local/main` to start it.

