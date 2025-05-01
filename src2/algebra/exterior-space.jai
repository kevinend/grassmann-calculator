/*
    // TODO: Move these into an Expression_Context struct or something
    // Then we can push and pop Expression_Context's which is how you change everything.
    scalar_variable_prefix_characters: string; // Expression_Context
    user_variables: Table(string, *Ast_Node);  // Expression_Context
*/

#scope_export

MAX_DIMENSION :: 5;

Exterior_Space :: struct {

    dimension: int;
    n_element: int;

    basis_element_symbols:        string;
    basis_elements:           []  Basis_Element;
    bitset_to_basis_elements: [] *Basis_Element;
}

init :: (space: *Exterior_Space, _1_elements: [] string) {

    dimension := _1_elements.count;
    assert(dimension <= MAX_DIMENSION);

    space.dimension                = dimension;
    space.n_element                = power(2, dimension)-1;
    space.basis_elements,
    space.basis_element_symbols    = generate_basis_elements(dimension, _1_elements);
    space.bitset_to_basis_elements = generate_bitset_to_basis_element_map(space.basis_elements);

    return;
}

get_dimension :: (space: *Exterior_Space) -> int {
    return space.dimension;
}

get_n_element :: (space: *Exterior_Space) -> int {
    return space.n_element;
}

get_basis_element :: (space: *Exterior_Space, bitset: int) -> *Basis_Element {
    basis_element := space.bitset_to_basis_elements[bitset];
    return basis_element;
}

deinit :: (space: *Exterior_Space) {
    
    // all fields of the space struct point to flat/contiguous buffers on the default heap.

    free(space.bitset_to_basis_elements.data);
    free(space.basis_elements.data);
    free(space.basis_element_symbols.data);
}

#scope_file
#import "Hash_Table";

power :: (a: int, b: int) -> int {

    // Implements the basic arithment expression: c = a^b
    c: int = 1;
    for i: 0..b-1 {
        c *= a;
    }
    return c;
}

choose :: (n: int, k: int) -> (value: int) {
    // example: choose(4,2) == (4*3)/(2*1) == 6
    if k == 0 return 1;
    return (n * choose(n-1, k-1)) / k;
}