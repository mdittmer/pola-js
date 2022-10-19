with (x) {
  (function(a) {
    with (y) {
      try {
        a;
      } catch (z) {
        a;
        b = null;
        z();
        var c;
      }
    }
    c = true;
  })();
}
