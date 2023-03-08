function x() {
  let z = {y: () => console.log('Hello dynamic')};

  with (z) {
    y();
  }

  function y() {
    console.log('Hello world');
  }


}

x();
