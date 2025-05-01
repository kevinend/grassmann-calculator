#scope_export

Factor :: struct {
    // Represents a single scalar factor like 'a1' on a basis term 'a1^e1'
    // The exponent determines whether it is a numerator (positive) or denominator (negative).
    exponent:        s32;
    scalar_variable: u32;
}

Term :: struct {
    value: float;
    #place value; multiples: float;

    // Factors include both numerators and denominators.
    // all 'count' fields *must* be signed to prevent underflow in for loops.
    factors: Relative_Pointer(Factor);
    factor_count: s16;                  

    previous: Relative_Pointer(Term);
    next:     Relative_Pointer(Term);
}

#scope_file