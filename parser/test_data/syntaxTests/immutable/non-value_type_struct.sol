contract Contract {
    struct S {
        int K;
    }

    S immutable s;
}
// ----
// TypeError 6377: (61-74): Immutable variables cannot have a non-value type.
