(function(a) {
  (function() {
    a.a;
    a.b = null;
    a.c();
    a();
  })();
  a;
})();
