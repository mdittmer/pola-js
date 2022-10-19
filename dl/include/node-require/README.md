# Node (the V8-based EcmaScript host environment) module-loading

This library layers on top of the `flow-sensitive-points-to` library to describe some of the semantics of the Node module loader. In broad strokes, the logic is concerned with the following:

- EcmaScript 3 scripts loaded in Node have a host object in their global scope bound to the name `module`

- When the top-level script finishes executing, the value of the above-mentioned host object's `exports` property is bound by Node as the interface provided to modules that load the above-mentioned script (via the host object/function `require()`)

- EcmaScript 3 scripts loaded in Node have a host object/function in their global scope bound to the name `require`

- Invoking `require(stringPathToModule)` triggers a module resolution algorithm that resolves `stringPathToModule` to a particular operating system file path, loads the file as EcmaScript source code, and returns the executed code's above-mentioned `module.exports` value

This library exposes several predicates that expose deduced information about `module.exports` values and `require()` invocations:

- `exitOutModuleExportsGraph(modulePath: symbol, exitNode: NodeID, src: PointToNode, edge: PointToEdge, sink: NextPointToNode)` (`flow-sensitive-points-to.dl`): Upon exiting the top-level function of the script located at `modulePath`, (via the flowgraph node `exitNode`), the `module.exports` property modeled by `src` may point to `sink`.

- `nodeRequire*` (`node-require.dl`): Several flavours of predicate associate a call node invoking the Node host object/function `require()` to its argument.

This library deduces a warning when it detects that a `require()` invocation with an argument that is not a string constant.
