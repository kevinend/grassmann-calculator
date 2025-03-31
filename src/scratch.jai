/* 
    Generating results for an expression *can* create a lot of temporary overhead.
    A fixed-sized 'scratch' space is allocated on application start-up. The limits are chosen based on expected usage, primarily
    with a Grassmann 4D space where each element in an expression is set with either the scalar 1 like '1*e1' or a single
    scalar variable like 'a1*e1'. With the current limits expressions such as (a1+a2+a3+a4+a5+a6+a7+a8)e1 and
    (a1a2a3a4..a16 + b1b2b3b4...b16 + c1c2c3c4...c16 + ..)e1 are possible.
    
    Depending on the expression, this space can sit 99% unused. It is the price of doing business. The application asserts
    that no more than 64Kib is being allocated to the space so we know up-front the upper limit of space reserved.
    
    Tweaking the max number of numerator or denominator factors only affects their total usage.
    Increasing the allowable max dimension of the space or the max number of terms allowed per coefficient *can* substantially
    grow the required scratch space. If it needs bumped up, then so be it, but the current configuration covers my intended
    use cases.
*/

#scope_export

MAX_TERMS_PER_BASIS_ELEMENT   :: 8;
MAX_NUMERATORS_PER_TERM       :: 16;
MAX_DENOMINATORS_PER_TERM     :: 16;

SCRATCH_TERMS                    :: (1                         * MAX_TERMS_PER_BASIS_ELEMENT * MAX_NUMBER_OF_BASIS_ELEMENTS);
SCRATCH_WORKING_SET_TERMS        :: (1                         * MAX_TERMS_PER_BASIS_ELEMENT * 1                           );

SCRATCH_NUMERATORS               :: (MAX_NUMERATORS_PER_TERM   * MAX_TERMS_PER_BASIS_ELEMENT * MAX_NUMBER_OF_BASIS_ELEMENTS);
SCRATCH_WORKING_SET_NUMERATORS   :: (MAX_NUMERATORS_PER_TERM   * MAX_TERMS_PER_BASIS_ELEMENT * 1                           );

SCRATCH_DENOMINATORS             :: (MAX_DENOMINATORS_PER_TERM * MAX_TERMS_PER_BASIS_ELEMENT * MAX_NUMBER_OF_BASIS_ELEMENTS);
SCRATCH_WORKING_SET_DENOMINATORS :: (MAX_DENOMINATORS_PER_TERM * MAX_TERMS_PER_BASIS_ELEMENT * 1                           );

TOTAL_SCRATCH_SPACE :: 
      SCRATCH_TERMS                     * size_of(Term)
    + SCRATCH_NUMERATORS                * size_of(Factor)
    + SCRATCH_DENOMINATORS              * size_of(Factor)
    + SCRATCH_WORKING_SET_TERMS         * size_of(Term)
    + SCRATCH_WORKING_SET_NUMERATORS    * size_of(Factor)
    + SCRATCH_WORKING_SET_DENOMINATORS  * size_of(Factor);

#assert(TOTAL_SCRATCH_SPACE < 65536);

Working_Set :: struct {
    // The working set represents the set of terms and factors belonging to a single basis element during evaluation.
    terms:          [SCRATCH_WORKING_SET_TERMS]         Term;
    numerators:     [SCRATCH_WORKING_SET_NUMERATORS]    Factor;
    denominators:   [SCRATCH_WORKING_SET_DENOMINATORS]  Factor;
}

Scratch :: struct {
    // The scratch buffers store the cumulative result from evaluation of terms and factors across the AST for each basis element.
    terms:         [SCRATCH_TERMS]        Term;
    numerators:    [SCRATCH_NUMERATORS]   Factor;
    denominators:  [SCRATCH_DENOMINATORS] Factor;
    working_set:   Working_Set;

    coefficients:  [MAX_NUMBER_OF_BASIS_ELEMENTS] Coefficient;
}

scratch: Scratch;
initialize_scratch_space :: (scratch: *Scratch) {
    // Space is statically allocated, just adjust the pointers for the coefficients to their respective sections of the scratch buffer
    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        scratch.coefficients[i].terms.data = *scratch.terms[i*MAX_TERMS_PER_BASIS_ELEMENT];
        scratch.coefficients[i].terms.count = 0;
    }

    // Iterate over the terms and set them to their respective numerator and denominators within the scratch space.
    for i: 0..SCRATCH_TERMS-1 {
        scratch.terms[i].numerators.data  = *scratch.numerators[i*MAX_NUMERATORS_PER_TERM];
        scratch.terms[i].numerators.count = 0;

        scratch.terms[i].denominators.data  = *scratch.denominators[i*MAX_DENOMINATORS_PER_TERM];
        scratch.terms[i].denominators.count = 0;
    }
}

reset_scratch_space :: (scratch: *Scratch) {
    // Do *not* adjust the pointers into the scratch space buffer, just reset counts back to zero.
    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        scratch.coefficients[i].terms.count = 0;
    }

    for i: 0..SCRATCH_TERMS-1 {
        scratch.terms[i].numerators.count = 0;
        scratch.terms[i].denominators.count = 0;
    }
}


#scope_file
