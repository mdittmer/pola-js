# Souffle datalog analysis

This directory souffle datalog source files for analyzing JavaScript source files. Input facts are modeled after the TAJS flowgraph representation of EcmaScript 3 source files.

The analysis traces dataflow to capture how read/write/invoke operations are exercised in the context of each EcmaScript function. Such analysis requires may-point-to analysis over properly scoped variables. Layered on top is a simple model of how Node interprets module imports and exports to capture contexts in which control may flow between modules. Taken together, these analyses can be applied to generate variable read/write/invoke policies for globals and function parameters that may be enforced by a custom module loader that wraps globals and parameters in proxies with interfaces limited to the authority actually needed by the interfaces between modules.
