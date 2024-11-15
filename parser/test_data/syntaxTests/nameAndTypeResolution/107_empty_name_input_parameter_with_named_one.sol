contract test {
    function f(uint, uint K) public returns (uint ret_k) {
        return K;
    }
}
// ----
// Warning 2018: (20-98): Function state mutability can be restricted to pure
