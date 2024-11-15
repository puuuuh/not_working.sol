contract test {
    struct S { uint x; }
    constructor(uint K) { S[K]; }
}
// ----
// TypeError 3940: (69-70): Integer constant expected.
