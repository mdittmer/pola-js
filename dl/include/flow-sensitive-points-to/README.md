# Flow-sensitive points-to analysis

This library defines an _extensive_, _monotone_ points-to analysis of TAJS flowgraph facts (building on scoping rules defined in the `flow-based-scoping` library). The analysis only ever _adds_ points-to edges as it analyzes dataflow between flowgraph nodes.

Nodes in the heap model, not to be confused with flowgraph nodes in the source code model, are values of the `PointToNode` type (`types.dl`). Edges are values of the type `PointToEdge` type (`types.dl`). A `nil` edge denotes a direct points-to relationship. All other edges contain a `Property` that describes a property name, denoting a object-property-points-to relationship. This model is rather loose, and technically admits invalid relationships such as `<$AbstractNode($NullNode)), $PropertyName($StringConstant("meaningOfLife")), $ConstantNode($NumberConstant(42))>`, which _would_ mean (in EcmaScript code) `(null).meaningOfLife` yields the value `42.0`, except that `null` cannot have property values.

## Points-to predicates

The main interface for this library are the following predicates:

- `outMayPointToWithCause(cause: NodeID, node: NodeID, source: Abstract, edge: PointToEdge, sink: PointToNode)`: On `OUT()` edges of flowgraph node `node`, the heap model contains may-point-to: `source --edge--> sink`. The flowgraph node that caused this may-point-to relationship is `cause`.

- `outMayPointTo(node: NodeID, source: Abstract, edge: PointToEdge, sink: PointToNode)`: The same as `outMayPointToWithCause`, but with no `cause`.

## Authority predicates

In addition to the classical may-point-to analysis, this library exposes an "authority tracking" analysis that captures the transitive of read/write/invoke authorities that are exercised via a particular variable. These authorities are exposed via the following predicates:

- `parameterAccess(function: FunctionID, src: Abstract, type: AuthorityType, property: AuthorityProperty, nextAuthority: NextAuthority)`: The heap model node described by `$AbstractNode(src)` is exercised in `type` mode (read, write, invoke, etc.) via `property` (e.g., `x.y()` is a method invocation on `x` via `y`), yielding a heap model value `nextAuthority` (which may lead to further exercising of authority). The graph described by all such `src --> nextAuthority` is rooted in `$AbstractNode($HeapNode($ParameterAllocation(f, v)))` heap model nodes; i.e., the network of exercised authority described by this predicate is authority flows that are "rooted in a function parameter variable".

- `globalAccess(src: Abstract, type: AuthorityType, property: AuthorityProperty, nextAuthority: NextAuthority)`: The same as `parameterAccess`, but with the network of exercised authority being authority flows that are "rooted in a global variable".
