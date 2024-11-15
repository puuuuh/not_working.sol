contract C {
  function f() public pure {
    assembly {
      K()
    }
  }
}
// ----
// DeclarationError 4619: (63-64): Function "K" not found.
