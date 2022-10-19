# Towards Least Authority in JavaScript

This project stitches together several JavaScript technologies to limit the authority of individual units of code.

# System requirements

This project requires the transitive dependencies of the following:

- [A fork of TAJS](https://github.com/mdittmer/TAJS/tree/pola-js#how-to-build-and-run-the-tool)

- [tajs-flowgraph-souffle](https://github.com/mdittmer/tajs-flowgraph-souffle)

Project scripts generally assume a shell environment running [BASH](https://www.gnu.org/software/bash/) 5.2.1 or newer.

# Building

```bash
# Update source dependencies in other repositories.
$ git submodule update --init --recursive
# Prime BASH environment variables.
$ source ./bash/env.sh
# Compile TAJS.
$ ./bash/tajs-compile.sh
```
