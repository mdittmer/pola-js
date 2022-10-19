# Flow-based scoping

This library binds variable references in source code to the scope that said variable _may_ belong to. The "_may_" part is necessary for EcmaScript source code because source such as `with (contextObject) { someVariable; }` will bind `someVariable` to `contextObject.someVariable` if it exists, or else some outer scope.

The "_flow-based_" part of this library is that flowgraph node/scope bindings are computed according to the `IN()` and `OUT()` edges of the TAJS flowgraph under analysis. The `IN()` case (`inScope(someFlowGraphNode, someScope)`) is used to deduce `mayVariableScopeBinding(variableName, someScope, someFlowGraphNode)` when variables are referenced in the flowgraph.

## TAJS Quirks and Errors

TAJS emits begin/end-with nodes, (usually associated with `with (expr) { ... }` syntax) around `try { ... }` blocks. This means that such nodes occur _a lot_ more frequently than the `with (expr) { ... }` syntax, which is generally discouraged in the JavaScript community due to its unpredictability.

The analysis assumes that all begin-with nodes have a source location that denotes a source path, which is important for accurate scoping logic. When this assumption is violated, an `error` predicate describing the violation is deduced.
