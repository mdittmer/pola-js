# Souffle tuning log

## 2023-01-25

Initial profile:

```bash
time `# Time performing the following...` \
  souffle `# Run souffle` \
  -c `# Compile` \
  -j 72 `# Jobs` \
  -F $POLA_JS_DIR/out/demo/aws-bookstore-demo-app/functions/APIs/node_modules/aws-sdk/clients/all.js/tajs/flowgraphs/facts `# Input facts` \
  -I $POLA_JS_DIR/dl/include `# Datalog source files include directory` \
  $POLA_JS_DIR/dl/node-require-source-target.dl `# Main datalog source file` \
  -D $POLA_JS_DIR/out/demo/aws-bookstore-demo-app/functions/APIs/node_modules/aws-sdk/clients/all.js/results `# Output directory` \
  `# -p $POLA_JS_DIR/out/demo/aws-bookstore-demo-app/funct/all.js/profile_c_j_72_no_frequency.prof \ # Profiling` \
  |& tee $POLA_JS_DIR/out/log/demo/aws-bookstore-demo-app/functions/APIs/node_modules/aws-sdk/clients/all.js/souffle.log # Souffle output log
```

- Most time going to `inMayPointToWithCause` and `outMayPointToWithCause`; not surprising

- Tried outputting only `nodeRequireSourceTarget`, not `warning` or `error

  - Run time the same (9-10min with `-j 72`)

-
