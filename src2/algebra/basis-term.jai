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
    basis:       *Basis_Element;
    coefficient: *Coefficient;
}

Complement_Type :: enum u8 {
    Left;
    Right;
}

// unary functions
negate :: (arg: *Basis_Term) -> *Basis_Term {
    // stub
    return null;
}

complement :: (type: Complement_Type, arg: *Basis_Term) -> *Basis_Term {
    // stub
    return null;
}

// binary functions
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