# Core library

This library is generally loaded first in the analysis. It defines input predicates that are emitted by TAJS fact-generating code and defines a few high-level internal predicates such as `error` and `warning` for deducing conditions that may render the analysis unsound or substantively less precise, respectively.

## Limitations and workarounds

- Line multiplier hack (`scoping.dl`, `errors.dl`): Logic is simplified by converting `(line, col)` pairs to a single number that is ordered according to the character offset in source files. TAJS does not supply a character offset, so a `line * multiplier + col` value is used, and an error is emitted if any `line` or `col` values are detected to be too large for this to work correctly.

- Outermost function adjustment (`scoping.dl`): TAJS stores the implicit function scope that wraps an entire script file with `(path, start_line, start_col, end_line, end_col) = (path, 0, 0, 0, 0)`. This breaks source-offset reasoning about containment, where scope-defining nodes are assumed to have `end_line` and `end_col` denoting the last source location where names would be considered in-scope.
