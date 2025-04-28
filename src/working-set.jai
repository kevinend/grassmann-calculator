/* 
    TODO: 04.26.2025 Do these comments still make sense?

    Generating results for an expression *can* create a lot of temporary/intermediate results.
    To mitigate against this growing out of control and using an unbounded amount of memory, fixed limits
    are imposed on the overall size of an individual coefficient belonging to a single basis term.

    The working set imposes the limit and can be adjusted if we run into calculations that exceed the
    bounds. The limit is intentionally restrictive so we can collect metrics and see what the actual
    usage of the application looks like. It is unlikely that we will hit the upper bounds on typical
    Grassmann calculations but we will see.

    // These limits don't mean that a single coefficient can *only* have 'X' number of terms. It can have way more terms but with fewer numerators and denominators.
// If a coefficient has multiple terms with tons of numerators and denominators then the coefficient *must* have fewer overall terms to meet the capacity limits.
// The total buffer *size* is fixed not the number of individual terms, numerators or denominators. 
// The constants are declared as upper-bounds on the number of each component of a coefficient as a reminder of how the buffer size limits were chosen the way that they are.
*/

#scope_export

SINGLE_COEFFICIENT_DATA_BUFFER_SIZE :: 512;

Working_Set :: struct {

    // The working set acts like a register file for this application.
    // Load data into the working set, perform operations, then write-back results after evaluation.

    NUM_BUFFERS :: 3;
    current_heap_index: int;
    heaps:   [NUM_BUFFERS] Bump_Heap;
    buffers: [NUM_BUFFERS][SINGLE_COEFFICIENT_DATA_BUFFER_SIZE] u8;

    // The 'aggregrate' section of the working set stores the latest coefficient for a basis term during evaluation of the AST.
    // Since not every expression uses every available basis term, keep track of which basis elements were part of the expression
    // to generate the final result.

    aggregate_coefficients: [MAX_NUMBER_OF_BASIS_ELEMENTS] *Coefficient;
    aggregate_heaps:        [MAX_NUMBER_OF_BASIS_ELEMENTS] Bump_Heap;
    aggregate_buffers:      [MAX_NUMBER_OF_BASIS_ELEMENTS] [SINGLE_COEFFICIENT_DATA_BUFFER_SIZE] u8;

    updated_basis_elements: [MAX_NUMBER_OF_BASIS_ELEMENTS] int;
    updated_basis_element_count: int;
}

initialize_working_set :: (ws: *Working_Set) {

    ws.current_heap_index = -1;
    for i: 0..Working_Set.NUM_BUFFERS-1 {
        buffer_start := *ws.buffers[i][0];
        buffer_end   := *ws.buffers[i][0] + SINGLE_COEFFICIENT_DATA_BUFFER_SIZE;
        memset(buffer_start, 0, SINGLE_COEFFICIENT_DATA_BUFFER_SIZE);

        heap_init(*ws.heaps[i], buffer_start, buffer_end);   
    }

    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        buffer_start := *ws.aggregate_buffers[i][0];
        buffer_end   := *ws.aggregate_buffers[i][0] + SINGLE_COEFFICIENT_DATA_BUFFER_SIZE;
        memset(buffer_start, 0, SINGLE_COEFFICIENT_DATA_BUFFER_SIZE);

        heap             := *ws.aggregate_heaps[i];
        heap_coefficient := *ws.aggregate_coefficients[i];

        heap_init(heap, buffer_start, buffer_end);
        heap_coefficient.* = allocate_coefficient(heap);
        heap_set_checkpoint(heap); // on reset make sure we leave the allocated coefficient for reuse
    }

    ws.updated_basis_element_count = 0;

    return;
}

reset_working_set :: (ws: *Working_Set) {

    ws.current_heap_index = -1;
    for i: 0..Working_Set.NUM_BUFFERS-1 {
        heap_reset_to_checkpoint(*ws.heaps[i]);
    }

    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        clear_coefficient(ws.aggregate_coefficients[i]);
        heap_reset_to_checkpoint(*ws.aggregate_heaps[i]);
    }

    ws.updated_basis_element_count = 0;

    return;
}

store_coefficient :: (ws: *Working_Set, coefficient: *Coefficient) -> *Coefficient {

    ws.current_heap_index = (ws.current_heap_index + 1) % Working_Set.NUM_BUFFERS;

    heap := *ws.heaps[ws.current_heap_index];
    heap_reset_to_checkpoint(heap);

    size := get_coefficient_size(coefficient);

    ws_coefficient := cast(*Coefficient)heap_allocate(heap, size, size_of(Coefficient));
    assert(ws_coefficient != null);
    memcpy(ws_coefficient, coefficient, size);

    ws_coefficient.heap = heap;
    
    return ws_coefficient;
}

load_coefficient :: (ws: *Working_Set, index: int) -> *Coefficient {
    
    // Loads *aggregate* coefficients from the working set.
    coefficient := ws.aggregate_coefficients[index];
    return coefficient;
}

// new_working_set_coefficient()
// new_working_set_term()
// new_working_set_factor()
// load_working_set_coefficient()
// which does the same thing as before but allocates in here instead!

// which just copies the same 


/*

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
    // Load data into the working set, perform operations, then write-back results after evaluation.

    NUM_BUFFERS :: 3;

    current_heap_index: int;
    buffers:      [NUM_BUFFERS] [512] u8;
    coefficients: [NUM_BUFFERS] 

    sums: Aggregate_Basis_Terms;
}





prepare_buffer :: (working_set: *Working_Set) -> Basis_Term {

    // rotate to next working set buffer.
    index := working_set.current_heap_index;
    index  = (index + 1) % Working_Set.NUM_BUFFERS;

    working_set.current_heap_index = index;

    // clear the working set buffer to zero.
    basis_term := working_set.basis_terms[0];
 //   copy_basis_term(src = zero(), dst = *basis_term);
    
    // return the basis term pointing to the active working set buffer for use in calculations.
    return basis_term;
}

get_aggregate_basis_term :: (working_set: *Working_Set, bitset: int) -> Basis_Term {
    return working_set.sums.basis_terms[bitset];
}

mark_aggregated_updated :: (working_set: *Working_Set, index: int) {

    i := working_set.sums.evaluated_basis_term_count;

    working_set.sums.evaluated_basis_terms[i]    = index;
    working_set.sums.evaluated_basis_term_count += 1;
}

working_set_to_ast :: (working_set: *Working_Set) -> Ast {

    for i: 0..working_set.sums.evaluated_basis_term_count-1 {
        index      := working_set.sums.evaluated_basis_terms[i];
        basis_term := working_set.sums.basis_terms[index];
        print("Basis term == %\n", basis_term.basis.symbol);
        print("Coefficient == %\n", coefficient_to_string(basis_term.coefficient));
    }


    ast: Ast;
    return ast;
}

*/

#scope_file



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

    memcpy(cast(*u8)scratch_coefficient.first_term, cast(*u8)input.first_term, input.byte_size);

    return scratch_coefficient;
}
*/

/*


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