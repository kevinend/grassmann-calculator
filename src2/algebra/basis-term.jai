/*
    Functions that operate on a basis term.
    Take a node like Basis_Term_Node, returns a list of the nodes that, intrinsics need a separate thingy
    position()  ; intrinsic
    direction() ; intrinsic
    measure()   ; intrinsic
*/

#scope_export

Basis_Term :: struct {
    // Represents a single basis element term in an expression such as 3e1 or (a1a2)e1^e2.
    bitset:       Basis_Bitset;
    coefficient: *Coefficient;
}

new_basis_term :: (bitset: Basis_Bitset, value: float) -> *Basis_Term {

    basis_term := cast(*Basis_Term)heap_allocate(context.expression_heap, size=size_of(Basis_Term), alignment= size_of(Basis_Term));
    assert(basis_term != null);

    coefficient := new_coefficient(value);
    
    basis_term.bitset = bitset;
    basis_term.coefficient = coefficient;

    return basis_term;
}

initialize_standard_basis_terms :: () {
    assert(context.expression_heap != null);
    ZERO = new_basis_term(bitset=0, value=1);
    ONE  = new_basis_term(bitset=0, value=1);
    return;
}

zero :: () -> *Basis_Term { return ZERO; }
one  :: () -> *Basis_Term { return ONE;  }

// unary operations
negate :: (arg: *Basis_Term) -> *Basis_Term {
    /*
        result := store_basis_term(arg); 
        foreach (term in result.coefficient.terms) {
            flip the sign 
        } 
        return result;
    */
    
    return null;
}

complement :: (left: bool, arg: *Basis_Term) -> *Basis_Term {
    // stub
    return null;
}

// binary operations
exterior_product :: (lhs: *Basis_Term, rhs: *Basis_Term) -> *Basis_Term {
    // stub
    return null;
}

regressive_product :: (lhs: *Basis_Term, rhs: *Basis_Term) -> *Basis_Term {
    // stub
    return null;
}

sum :: (lhs: *Basis_Term, rhs: *Basis_Term) -> *Basis_Term {
    // stub
    return null;
}

#scope_file

ZERO: *Basis_Term;
ONE:  *Basis_Term;