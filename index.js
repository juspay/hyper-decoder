(() => {
  // output/Main/index.js
  var temp = function(x) {
    return x + 1 | 0;
  };
  var testfun = function(inp) {
    return {
      ...inp,
      a: 10 + temp(inp.a) | 0,
      b: 6,
      d: {
        ...inp.d,
        t: 19
      }
    };
  };
  var main = /* @__PURE__ */ testfun({
    a: 10,
    b: 11,
    c: 12,
    d: {
      t: 13
    }
  });

  // <stdin>
  main();
})();
