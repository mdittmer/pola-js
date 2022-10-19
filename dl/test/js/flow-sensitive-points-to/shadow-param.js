(function(a) {
  (function(a) {
    a;
    a();
    a = null;
  })();
  a.b;
  a.c = true;
  a.d();
  delete a;
})();
