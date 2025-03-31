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

MAX_TERMS_PER_COEFFICIENT   :: 8;
MAX_NUMERATORS_PER_TERM     :: 16;
MAX_DENOMINATORS_PER_TERM   :: 16;

SCRATCH_TERMS                    :: (1                         * MAX_TERMS_PER_COEFFICIENT * MAX_NUMBER_OF_BASIS_ELEMENTS);
SCRATCH_WORKING_SET_TERMS        :: (1                         * MAX_TERMS_PER_COEFFICIENT * 1                           );

SCRATCH_NUMERATORS               :: (MAX_NUMERATORS_PER_TERM   * MAX_TERMS_PER_COEFFICIENT * MAX_NUMBER_OF_BASIS_ELEMENTS);
SCRATCH_WORKING_SET_NUMERATORS   :: (MAX_NUMERATORS_PER_TERM   * MAX_TERMS_PER_COEFFICIENT * 1                           );

SCRATCH_DENOMINATORS             :: (MAX_DENOMINATORS_PER_TERM * MAX_TERMS_PER_COEFFICIENT * MAX_NUMBER_OF_BASIS_ELEMENTS);
SCRATCH_WORKING_SET_DENOMINATORS :: (MAX_DENOMINATORS_PER_TERM * MAX_TERMS_PER_COEFFICIENT * 1                           );

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
    coefficient:    Coefficient;
}

Scratch :: struct {
    // The scratch buffers store the cumulative result from evaluation of terms and factors across the AST for each basis element.
    terms:         [SCRATCH_TERMS]        Term;
    numerators:    [SCRATCH_NUMERATORS]   Factor;
    denominators:  [SCRATCH_DENOMINATORS] Factor;
    working_set:   Working_Set;

    coefficients:    [MAX_NUMBER_OF_BASIS_ELEMENTS] Coefficient;
    populated_slots: [MAX_NUMBER_OF_BASIS_ELEMENTS] int;
    populated_count: int;
}

initialize_scratch_space :: (scratch: *Scratch) {
    // Space is statically allocated, just adjust the pointers for the coefficients to their respective sections of the scratch buffer
    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        scratch.coefficients[i].terms.data = *scratch.terms[i*MAX_TERMS_PER_COEFFICIENT];
        scratch.coefficients[i].terms.count = 0;
    }

    // Iterate over the terms and set them to their respective numerator and denominators within the scratch space.
    for i: 0..SCRATCH_TERMS-1 {
        scratch.terms[i].numerators.data  = *scratch.numerators[i*MAX_NUMERATORS_PER_TERM];
        scratch.terms[i].numerators.count = 0;

        scratch.terms[i].denominators.data  = *scratch.denominators[i*MAX_DENOMINATORS_PER_TERM];
        scratch.terms[i].denominators.count = 0;
    }

    // Initialize working set under the scratch space.
    // Working set contains a single coefficient.
    scratch.working_set.coefficient.terms.data  = scratch.working_set.terms.data;
    scratch.working_set.coefficient.terms.count = 0;

    // Each term in the working set should point to its respective factors (numerators and denominators).
    for i: 0..MAX_TERMS_PER_COEFFICIENT-1 {
        scratch.working_set.terms[i].numerators.data  = *scratch.working_set.numerators[i*MAX_NUMERATORS_PER_TERM];
        scratch.working_set.terms[i].numerators.count = 0;

        scratch.working_set.terms[i].denominators.data  = *scratch.working_set.denominators[i*MAX_DENOMINATORS_PER_TERM];
        scratch.working_set.terms[i].denominators.count = 0;
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

    // Working set is constantly overwritten during evaluation, no need to reset it.

    scratch.populated_count = 0;
}

load_coefficient :: (scratch: *Scratch, index: int) -> *Coefficient {
    return *scratch.coefficients[index];
}

store_then_load_coefficient :: (working_set: *Working_Set, coefficient: Coefficient) -> *Coefficient {

    for i: 0..coefficient.terms.count-1 {
        working_set.terms[i] = coefficient.terms[i];
        memcpy(working_set.terms[i].numerators.data,   coefficient.terms[i].numerators.data,   coefficient.terms[i].numerators.count   * size_of(Factor));
        memcpy(working_set.terms[i].denominators.data, coefficient.terms[i].denominators.data, coefficient.terms[i].denominators.count * size_of(Factor));
    }

    working_set.coefficient.terms.count = coefficient.terms.count;

    return *working_set.coefficient;
}

update_scratch_coefficient :: (coefficient: *Coefficient, src: *Term) #no_abc {

    // Coefficient belongs to the scratch space. Add the new term to the list of terms.
    // 
    // #no_abc is set to handle when the coefficient has zero terms.
    // Each coefficient in scratch space points to valid memory but the terms.count == 0 so you can't index the array view without the #no_abc.

    assert(coefficient.terms.count < MAX_TERMS_PER_COEFFICIENT);

    i   :=  coefficient.terms.count;
    dst := *coefficient.terms[i];

    memcpy(dst, src, size_of(Term));
  
    // Copy the term's factors and denominators.
    memcpy(dst.numerators.data,   src.numerators.data,   src.numerators.count   * size_of(Factor));
    memcpy(dst.denominators.data, src.denominators.data, src.denominators.count * size_of(Factor));

    dst.numerators.count   = src.numerators.count;
    dst.denominators.count = src.denominators.count;

    coefficient.terms.count += 1;
    return;
}

mark_coefficient_populated :: (scratch: *Scratch, index: int) {

    scratch.populated_slots[scratch.populated_count] = index;
    scratch.populated_count += 1;
}

scratch_to_ast :: (scratch: *Scratch) -> *Ast_Node {

    // TODO: Make this work for permanent results!

    bitset:       int           = 0;
    coefficient:  Coefficient   = zero();
    basis:       *Basis_Element = null;

    if scratch.populated_count == 1 {
        bitset      = scratch.populated_slots[0];
        coefficient = scratch.coefficients[bitset];
        basis       = get_basis_element(bitset);

		root := new_element_node(0, coefficient, basis);
        return root;
	}
	else {
		root := new_binary_operator_node(0, null, .Addition, null);
		i: int = 0;
		current := root;
		while i < scratch.populated_count-2 {
            bitset      = scratch.populated_slots[i];
            coefficient = scratch.coefficients[bitset];
            basis       = get_basis_element(bitset);

			current.lhs = new_element_node(0, coefficient, basis);
			next := new_binary_operator_node(0, null, .Addition, null);
			current.rhs = next;
			current     = next;

			i += 1;
		}

        bitset       = scratch.populated_slots[i];
        coefficient  = scratch.coefficients[bitset];
        basis        = get_basis_element(bitset);

		current.lhs  = new_element_node(0, coefficient, basis);

        bitset       = scratch.populated_slots[i+1];
        coefficient  = scratch.coefficients[bitset];
        basis        = get_basis_element(bitset);

		current.rhs = new_element_node(0, coefficient, basis);

        return root;
	}
}