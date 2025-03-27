# Orb

A Haskell web framework based on Shrubbery and Wai.

## What is Orb?

Orb is a Haskell web framework that aims to provide a straightforward approach
to building web applications. Inspired by the principles of Simple Haskell, Orb
emphasizes simplicity, and tries to minimize the user's exposure to advanced
language extensions and type-level programming while leveraging the benefits
those features provide. Associated types in type classes such as `HasHandler`
ensure strong compile-time guarantees while maintaining readability, clarity,
and pluggability.

Orb enables type-safe routing, request handling, and response management. The
framework intentionally trades conciseness for explicitness, preferring clear
and verbose structures to avoid unexpected action at a distance. Orb is
designed with the additional goal of facilitating automatic generation of
OpenAPI specifications, which we hope to implement in the near future.

## Working on Orb

You will need a working "rootless Docker" setup. See
[here for more information](https://docs.docker.com/engine/security/rootless/).

### Run the bootstrap script

The first time you clone the repo you'll need to run the bootstrap script.

```bash
./scripts/bootstrap
```

This will configure your local environment (Docker volume, `.env` file, etc)
required for the development environment to run.

### Do an initial build

```bash
./scripts/build
```

This is the script you should run to make sure everything is ship-shape before
creating a PR, and can be useful to run after pulling to ensure everything is
up to date.

### Getting started with development

You can build Orb for development using the following command:

```bash
./scripts/test
```

This will build and test the code. If you want to build the code automatically
on a file change, you can run:

```bash
./scripts/test --file-watch
```

You may sometimes need to start the container with a broken build, or just want
to jump straight into a `stack repl` rather than waiting around for the normal
`stack build` to complete.

In this case, it's usually more convenient to use the shell script instead.

To create a new dev container, use:

```bash
./scripts/shell
```

This will drop you into a bash shell in the project directory. From here, you
can run `stack repl` or other stack commands immediately.

Or, if you'd like less verbose compiler output:

```bash
stack repl --ghc-options="-v0"
```

Another option that might be nice to set is one that limits the memory that GHC
will use while in the REPL.

```bash
stack repl --ghc-options="+RTS -M1.5G"
```

This will load the development environment at the REPL. Once at the REPL, you
can use `:r` to reload the code.

