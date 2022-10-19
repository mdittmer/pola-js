var x;
(function(a) {
  x = a;
})();
x.a;
x.b = null;
x.c();
x();
