contract C {
  function f() public pure {
    assembly {
      function K() {}

      K
    }
  }
}
// ----
// ParserError 6913: (92-93): Call or assignment expected.
