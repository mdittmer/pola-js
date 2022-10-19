// Using node version "vX.Y.Z":
// `node ./gen-builtin-module-facts.js > ./facts/nodejs-vX.Y.Z/coreNodeModule.facts`.
var process = require('process');
var builtinModules = require('module').builtinModules;
for (var i = 0; i < builtinModules.length; i++) {
  process.stdout.write('"' + builtinModules[i] + '"\n');
}
