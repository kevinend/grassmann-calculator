/*
    Evaluation/simplification of the AST.
    Occurs in two steps:
    1. Evaluate all "local" operations. This is everything except for addition and subtraction
    2. Evaluate addition and subtraction since this needs to occur across nodes in the tree.

    Evaluation has to handle the anticommutativity of the exterior and regressive products.
    The visual below is important to keep in mind otherwise the code will seem backwards.

    In a product between two arguments the right-most element of the left-hand side (LHS) is *next to* the left-most
    element of the right-hand side (RHS). In code this is not the case and needs accounted for otherwise calculating
    anticommutativity between elements due to 'swaps' will not be correct.

    In the LHS, the right-most basis element factor is stored in the MSB.
    In the RHS, the left-most  basis element factor is stored in the LSB.

		LHS              RHS
	(e0^e1^e2^e3) 	v 	(e0^e1^e2^e3)
               ^      	 ^
		       |      	 |
    \/---------|      	 |--------\/
	e3 e2 e1 e0		 	 e3 e2 e1 e0
	^                             ^
    MSB of LHS                    LSB of RHS
*/

#scope_export

simplify :: (ast: Ast) -> Ast {

    reduced: Ast;
    reduced.root = eval(ast.root);
    return reduced;
}

reset_scratch :: () {

    memset(*scratch.buffer.data, 0, Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS * size_of(*Term));
    scratch.count = 0;

    return;
}

#scope_file

Complement_Type :: enum {
    Left;
    Right;
}

Scratch :: struct {

    // Scratch space is used to 'flatten' the tree after evaluation of other operations.
    // Since we know the canonical ordering for the elements we can take a node from the tree, figure out it's 'display' location
    // then use that same index to sum all other leaves in the tree that point to the same element.

    buffer: [Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS] *Term;
    count: int;
}

scratch: Scratch;

eval :: (node: *Ast_Node) -> *Ast_Node {

    // Depth-first search of the AST.
    // Evaluates all 'local' operations, so everything excluding addition and subtraction.

    if node.kind == .Element {
        return node;
    }

    result: *Ast_Node = null;
    if node.kind == .Unary_Operator {
        unary_op    := cast(*Unary_Operator_Node)node;
        unary_op.rhs = eval(unary_op.rhs);

        assert(unary_op.rhs.kind == .Element);
        arg := cast(*Element_Node)unary_op.rhs;

        if unary_op.op == {
            case .Negation;         { result = negate(arg); }
            case .Left_Complement;  { result = complement( Complement_Type.Left,  arg); }
            case .Right_Complement; { result = complement( Complement_Type.Right, arg); }
        }
    }

   // assert(result != null);
    return result;

}

negate :: (element: *Element_Node) -> *Ast_Node {

    assert(element != null);
    
    scale_term(element.coefficient, -1);
    result := new_element_node(element.node_id, element.coefficient, element.basis);
    return result;
}

complement :: (type: Complement_Type, k: *Element_Node) -> *Ast_Node {

    // Generates an (n-k)-element from the input k-element.

    bits        := space.n_element.bits ^ k.basis.bits;
    basis       := get_basis_element(bits);
    coefficient := k.coefficient;

    node := new_element_node(k.node_id, coefficient, basis);

    anticommute: bool = false;
    if type == .Left {
        anticommute = does_product_anticommute(bits, k.basis.bits);
    }
    else
    if type == .Right {
        anticommute = does_product_anticommute(k.basis.bits, bits);
    }

    if anticommute == true {
        // scale the coefficient!
    }

    return node;
}

does_product_anticommute :: (lhs: int, rhs: int) -> bool {
    return true;
}