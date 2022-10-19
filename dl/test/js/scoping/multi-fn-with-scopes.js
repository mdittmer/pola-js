// Check that function declarations nest inside with scopes appropriately.
with (x) { // with:2:1
  function f(a) { with (y) { // function:3:3 ; with:3:19
    function g(b) { with (z) { // function:4:5 ; with:4:21
query:    c;
        }
      }
      var c;
    } } }
