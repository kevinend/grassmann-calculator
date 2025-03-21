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

    simplified: Ast;

    reset_scratch();
    simplified.root = eval(ast.root);
    sum(simplified.root);

    for i: 0..Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        if scratch.buffer[i] != null {
            print("Value == %\n", term_to_string(scratch.buffer[i]));
        }
    }
    return simplified;
}

reset_scratch :: () {

    memset(scratch.buffer.data, 0, Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS * size_of(*Term));
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

    // Evaluates all 'local' operations, so everything excluding addition and subtraction.

    result: *Ast_Node = null;
    if node.kind == .Element {
        result = node;
    }
    else
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
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        binary_op.lhs = eval(binary_op.lhs);
        binary_op.rhs = eval(binary_op.rhs);

        // assume binary operator is a Grassmann product to create short-hand element node vars ahead of time.
        lhs := cast(*Element_Node)binary_op.lhs;
        rhs := cast(*Element_Node)binary_op.rhs;

        if binary_op.op == {
            case .Scalar_Multiplication; { result = scalar_multiplication(lhs, rhs);   }
            case .Exterior_Product;      { result = exterior_product(lhs, rhs);   }
            case .Regressive_Product;    { result = regressive_product(lhs, rhs); }
            case; {
                // operator is sum or difference. defer until later step.
                result = binary_op;
            }
        }
    }

    // This function should only return basis element nodes and addition and subtraction operator nodes.
    return result;

}

negate :: (element: *Element_Node) -> *Element_Node {

    assert(element != null);
    
    scale_term(element.coefficient, -1);
    result := new_element_node(element.node_id, element.coefficient, element.basis);
    return result;
}

complement :: (type: Complement_Type, k: *Element_Node) -> *Element_Node {

    // Generates an (n-k)-element from the input k-element.

    bits        := space.n_element.bitset ^ k.basis.bitset;
    basis       := get_basis_element(bits);
    coefficient := k.coefficient;

    n_k := new_element_node(k.node_id, coefficient, basis);

    anticommute: bool = false;
    if type == .Left {
        anticommute = does_product_anticommute(n_k.basis.bitset, k.basis.bitset);
    }
    else
    if type == .Right {
        anticommute = does_product_anticommute(k.basis.bitset, n_k.basis.bitset);
    }

    if anticommute == true {
        scale_term(n_k.coefficient, -1);
    }

    return n_k;
}

scalar_multiplication :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    assert(lhs.basis.grade == 0 || rhs.basis.grade == 0);

    coefficient := new_term(0);
    result      := new_element_node(0, coefficient, get_basis_element(0));

    result.coefficient = multiply_terms(lhs.coefficient, rhs.coefficient);
    result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);

    return result;
}

exterior_product :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    coefficient := new_term(0);
    result      := new_element_node(0, coefficient, get_basis_element(0));

    if lhs.basis.grade == 0 || rhs.basis.grade == 0 {
        // scalar multiplication
        result.coefficient = multiply_terms(lhs.coefficient, rhs.coefficient);
        result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);
    }
    else
    if (lhs.basis.bitset & rhs.basis.bitset) == 0 {
        // no one elements in common; implies sum of grades is less than or equal to N.
        result.coefficient = multiply_terms(lhs.coefficient, rhs.coefficient);
        result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);

        anticommute := does_product_anticommute(lhs.basis.bitset, rhs.basis.bitset);
        if anticommute {
            scale_term(result.coefficient, -1);
        }
    }
    // else no one element in common, result is zero.

    return result;
}

regressive_product :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    coefficient := new_term(0);
    result      := new_element_node(0, coefficient, get_basis_element(0));
    if (lhs.basis.bitset | rhs.basis.bitset) == space.n_element.bitset {

        result.basis = get_basis_element(lhs.basis.bitset & rhs.basis.bitset);
        if result.basis.bitset == 0 {
            // The lhs and rhs are complements of one another generating a zero element.
            result.coefficient = multiply_terms(lhs.coefficient, rhs.coefficient);

            anticommute := does_product_anticommute(lhs.basis.bitset, rhs.basis.bitset);
            if anticommute {
                scale_term(result.coefficient, -1);
            }
        }
        else {
            result.coefficient = multiply_terms(lhs.coefficient, rhs.coefficient);

            anticommute1 := does_product_anticommute(lhs.basis.bitset ^ result.basis.bitset, result.basis.bitset);
            anticommute2 := does_product_anticommute(result.basis.bitset, rhs.basis.bitset);

            if anticommute1 != anticommute2 {
                scale_term(result.coefficient, -1);
            }
        }
    }
    // else arguments do *not* generate at least the n-element so the result is zero.

    return result;
}

does_product_anticommute :: (lhs: int, rhs: int) -> bool {

    // Steps:
    //  Start at the right-most bit of the LHS (check diagram in header comment at start of the file if this doesn't make sense!)
    //  Iterate through each bit in the LHS.
    //   If the bit is set in the LHS then find it's corresponding location (bit) in the RHS.
    //   Once we reach the location in the RHS set the bit.
    //   During the move, if you cross over a 'set' bit in the RHS then you have swapped elements.
    //
    // If at the end you've swapped an odd number of times then the product anticommutes.

    temp := rhs;
    swaps: int = 0;

    for #v2 < i: 0..space.dimension-1 {
        if bit_is_set(lhs, i) {
            for j: 0..space.dimension-1 {
                if i == j {
                    set_bit(*temp, j);
                    break;
                }
                else
                if bit_is_set(temp, j) {
                    swaps += 1;
                }
            }
        }
    }

    return (swaps % 2 != 0);
}

sum :: (node: *Ast_Node) {
    
    // search the AST for basis elements and add each into it's location in the scratch buffer.
    if node.kind == .Element {
        element := cast(*Element_Node)node;
        index   := get_basis_element_index(element.basis.bitset);

        print("index == %\n", index);

        term := scratch.buffer[index];
        result := combine_like_terms(element.coefficient, term);

        scratch.buffer[index] = result;
    }
    else
    if node.kind == .Binary_Operator {
        // TODO: Assumes only addition! not subtraction here!
        binary_op := cast(*Binary_Operator_Node)node;
        sum(binary_op.lhs);
        sum(binary_op.rhs);
    }
    
    return;
}

// Utility functions for Grassmann products that operate on individual bits.
bit_is_set :: inline (bitset: int, bit: int) -> bool {
    return (bitset & (1 << bit)) != 0;
}

set_bit :: inline (bitset: *int, bit: int) {
    bitset.* |= (1 << bit);
}