# Towards Least Authority in JavaScript

This project stitches together several JavaScript technologies to limit the authority of individual units of code.

## System requirements

This project requires the transitive dependencies of the following:

- [A fork of TAJS](https://github.com/mdittmer/TAJS/tree/pola-js#how-to-build-and-run-the-tool)

  - Vendored in `TAJS` submodule

- [Souffle datalog](https://github.com/mdittmer/tajs-flowgraph-souffle)

  - Vendored in `souffle` submodule

Project scripts generally assume a shell environment running [BASH](https://www.gnu.org/software/bash/) 5.2.1 or newer.

This project optionally requires the transitive dependencies of the following:

- [swc](https://github.com/swc-project/swc) (for working with "modern" JavaScript code via cross-compilation)

  - Vendored in `swc` submodule

## Building

```bash
# Update source dependencies in other repositories.
$ git submodule update --init --recursive
# Prime BASH environment variables.
$ source ./bash/env.sh
# Compile TAJS.
$ ./bash/tajs-compile.sh
# Compile souffle.
$ ./bash/souffle-compile.sh
# Compile swc (may not be required, depending on use case).
$ ./bash/swc-compile.sh
```

## Run tests

```bash
# Run tests that test properties of the output from:
#
# Reference JavaScript
#     --TAJS--> Souffle datalog facts
# Souffle datalog facts + Souffle datalog test code
#     --Souffle--> Test output
$ ./bash/test-js.sh
```

## Work with example code

```bash
# Use npm to install dependencies of demo code.
$ ./bash/install-demo-code.sh
```
