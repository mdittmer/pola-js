# Souffle datalog include directory

This directory is used in `-I <dir>` designations in invocations of `souffle`.

Includes are modularized as follow:

`<include-dir>/(<super-module>/)*<module>/<module>.dl` defines the public
interface to be consumed exactly once by out-of-module code. This convention
suggests (though does not guarantee) that `<name>.dl` files are unique, which
is helpful given that Souffle uses file names (not paths) in its error messages.

All other `.dl` files should be included exactly once by their local
`<module>.dl` file.
