# dev-local

Create a local development environment for testing.

**NOTE:** All the following commands mentioned need to be executed from the root
of the repository.

## Setup

You'll first need to run the setup script once to ensure a few needed
executables exist.

**NOTE:** You'll need to run this only once and make sure it succeeds.

```
./dev-local/setup
```

## Creating the env

The following command would then setup the local environment. If the setup is
without any issues this command should create the required processes to setup
the local environment.

```
./dev-local/main
```

**NOTE:** Sometimes it is possible that the processes may not shut down
properly. In that case, some manual cleanup may be needed.
