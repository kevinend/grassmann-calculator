/* 
    Generating results for an expression *can* create a lot of temporary/intermediate results.
    To mitigate against this growing out of control and using an unbounded amount of memory, fixed limits
    are imposed on the overall size of an individual coefficient belonging to a single basis term.

    The working set imposes the limit and can be adjusted if we run into calculations that exceed the
    bounds. The limit is intentionally restrictive so we can collect metrics and see what the actual
    usage of the application looks like. It is unlikely that we will hit the upper bounds on typical
    Grassmann calculations but we will see.
*/

#scope_export

// These limits don't mean that a single coefficient can *only* have 'X' number of terms. It can have way more terms but with fewer numerators and denominators.
// If a coefficient has multiple terms with tons of numerators and denominators then the coefficient *must* have fewer overall terms to meet the capacity limits.
// The total buffer *size* is fixed not the number of individual terms, numerators or denominators. 
// The constants are declared as upper-bounds on the number of each component of a coefficient as a reminder of how the buffer size limits were chosen the way that they are.

MAX_TERMS_PER_COEFFICIENT   :: 16;
MAX_NUMERATORS_PER_TERM     :: 15;
MAX_DENOMINATORS_PER_TERM   :: 15;

#assert(size_of(Term)        <= 8);
#assert(size_of(Numerator)   <= 4);
#assert(size_of(Denominator) <= 4);

SINGLE_COEFFICIENT_DATA_BUFFER_SIZE :: 
      MAX_TERMS_PER_COEFFICIENT * size_of(Term)
    + MAX_TERMS_PER_COEFFICIENT * (MAX_NUMERATORS_PER_TERM   * size_of(Numerator))
    + MAX_TERMS_PER_COEFFICIENT * (MAX_DENOMINATORS_PER_TERM * size_of(Denominator));

#assert(1                            * SINGLE_COEFFICIENT_DATA_BUFFER_SIZE <= 2048 );
#assert(MAX_NUMBER_OF_BASIS_ELEMENTS * SINGLE_COEFFICIENT_DATA_BUFFER_SIZE <= 65536);

Aggregate_Basis_Terms :: struct {
    
    // Stores the latest coefficient for a basis term during evaluation of the AST.
    // Since not every expression uses every available basis term, keep track of which basis elements were part of the expression
    // to generate the final result.

    buffer:      [MAX_NUMBER_OF_BASIS_ELEMENTS] [SINGLE_COEFFICIENT_DATA_BUFFER_SIZE] u8;
    basis_terms: [MAX_NUMBER_OF_BASIS_ELEMENTS] Basis_Term;

    // Track the indices of basis terms in basis terms array that have updated coefficients during evaluation.
    evaluated_basis_terms: [MAX_NUMBER_OF_BASIS_ELEMENTS] int;
    evaluated_basis_term_count: int;
}

Working_Set :: struct {

    // The working set acts like a register file for this application.
    // Load basis terms and their coefficient's into the working set, peform operations, then write-back results after evaluation.
    // Write-back occurs against the 'sums' to track the latest value for a basis element during evaluation.

    NUM_BUFFERS :: 3;

    current_buffer_index: int;
    buffers:     [NUM_BUFFERS] [SINGLE_COEFFICIENT_DATA_BUFFER_SIZE] u8;
    basis_terms: [NUM_BUFFERS] Basis_Term;

    sums: Aggregate_Basis_Terms;
}

initialize_working_set :: (working_set: *Working_Set) {

    // Initialization needs to set/adjust pointers to the appropriate sections of the statically allocated working set space.
    
    // Initialize working buffers for intermediate calculations.
    working_set.current_buffer_index = -1;
    for i: 0..Working_Set.NUM_BUFFERS-1 {
        working_set.basis_terms[i].basis       = get_basis_element(0);
        working_set.basis_terms[i].coefficient = cast(*Coefficient)*working_set.buffers[i];
        memset(working_set.basis_terms[i].coefficient, 0, size_of(Coefficient));
    }

    // Initialize basis terms sums.
    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        working_set.sums.basis_terms[i].basis = get_basis_element(i);
        working_set.sums.basis_terms[i].coefficient = cast(*Coefficient)*working_set.sums.buffer[i];
        memset(working_set.sums.basis_terms[i].coefficient, 0, size_of(Coefficient));
    }
}

reset_working_set :: (working_set: *Working_Set) {
    
    // Reset the coefficients in the ring of buffers (but not quite a ring buffer...)
    // Do *not* adjust pointers that were set on initialization!!!

    working_set.current_buffer_index = -1;
    working_set.sums.evaluated_basis_term_count = 0;

    for i: 0..Working_Set.NUM_BUFFERS-1 {
        working_set.basis_terms[i].basis = get_basis_element(0);
        memset(working_set.basis_terms[i].coefficient, 0, size_of(Coefficient));
    }

    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        memset(working_set.sums.basis_terms[i].coefficient, 0, size_of(Coefficient));
    }
}

/*

rotate_active_scratch_buffer :: inline (working_set: *Working_Set) {
    working_set.current_buffer_index = (working_set.current_buffer_index + 1) % Working_Set.NUM_SCRATCH_BUFFERS;
    return;
}

get_scratch_cofficient :: inline (working_set: *Working_Set) -> Coefficient {
    index := working_set.current_buffer_index;
    return working_set.scratch_coefficients[index];
}

load_coefficient_into_working_set :: (working_set: *Working_Set, input: Coefficient) -> Coefficient {
    
    // Copy the contents of the input coefficient into the 'active' scratch buffer.
    // TODO: This is janky not taking pointers or returning pointers...04.12.2025
    
    index := working_set.current_buffer_index;

    ptr_scratch_coefficient             := *working_set.scratch_coefficients[index];
    ptr_scratch_coefficient.term_count   = input.term_count;
    ptr_scratch_coefficient.byte_size    = input.byte_size;

    scratch_coefficient := working_set.scratch_coefficients[index];

    memcpy(cast(*u8)scratch_coefficient.terms, cast(*u8)input.terms, input.byte_size);

    return scratch_coefficient;
}
*/

/*
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