(function(a) {
  a.b = unknownVariable;
  a.c = unknownVariable.unknownProperty;
  a.b;
  a.c;
})();
