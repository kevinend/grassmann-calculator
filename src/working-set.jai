/* 
    Generating results for an expression *can* create a lot of temporary/intermediate results.
    To mitigate against this growing out of control and using an unbounded amount of memory, fixed limits
    are imposed on the overall size of an individual coefficient belonging to a single basis element.

    The working set imposes the limit and can be adjusted if we run into calculations that exceed the
    bounds. The limit is intentionally restrictive so we can collect metrics and see what the actual
    usage of the application looks like. It is unlikely that we will hit the upper bounds on typical
    Grassmann calculations but we will see.
*/

#scope_export

// These limits don't mean that a single coefficient can *only* have 'X' number of terms. It can have way more terms but with fewer numerators and denominators.
// If a coefficient has multiple terms with tons of numerators and denominators then the coefficient *must* have fewer overall terms to meet the capacity limits.
// The buffer *size* is fixed not the number of individual terms, numerators or denominators. 
// The constants are declared as upper-bounds on the number of each component of a coefficient as a reminder of how the buffer size limits were chosen the way that they are.

MAX_TERMS_PER_COEFFICIENT   :: 16;
MAX_NUMERATORS_PER_TERM     :: 15;
MAX_DENOMINATORS_PER_TERM   :: 15;

#assert(size_of(Term)        <= 8);
#assert(size_of(Numerator)   <= 4);
#assert(size_of(Denominator) <= 4);

// The scratch buffer stores the coefficient for a *single* basis element.
SCRATCH_BUFFER_SIZE :: 
      MAX_TERMS_PER_COEFFICIENT * size_of(Term)
    + MAX_TERMS_PER_COEFFICIENT * (MAX_NUMERATORS_PER_TERM * size_of(Numerator))
    + MAX_TERMS_PER_COEFFICIENT * (MAX_DENOMINATORS_PER_TERM * size_of(Denominator));

// Total scratch space *assumes* that each and every basis element has a coefficient that uses the maximum space.
TOTAL_SCRATCH_SPACE :: MAX_NUMBER_OF_BASIS_ELEMENTS  * SCRATCH_BUFFER_SIZE;

#assert(SCRATCH_BUFFER_SIZE <= 2048);
#assert(TOTAL_SCRATCH_SPACE <= 65536);

// TOOD: 04.09.2025 -- Time to get after it kiddo. This is where the meat and potatoes are made!
// As discussed before we need to introduce the top-level 'eval' loop so that this fits into the picture cleanly.
// The idea is that we have 3 buffers.
//  2 of them are used as 'swap' for intermediate calculations 2048 x 2
//  1 of them is the accumulator for each basis element. 65536
//
// We need the eval loop to load into the active buffer, use it for the calculation then flip back over
// Then for the sum we have the existing sum buffer for that coefficient, we have the intermediate result then write to the active buffer
// Then write-back to the sum buffer.
//  Load
//  Decode
//  Execute
//  Memory
//  Write-back...sort of!
// This is a tomorrow problem but I am actually excited to get back to it, should be fun enough to do and with the flatten I feel WAY better
// about things.


/* TODO: Remove this code below, this is just for reference 
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

*/