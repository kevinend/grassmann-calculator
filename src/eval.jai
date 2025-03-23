/*
    Evaluation/simplification of the AST occurs in two steps.
        1. Evaluate all "local" operations.
        2. Evaluate addition/summation.

    Products and unary operators are 'local' operations since while traversing the tree you can evaluate their children
    then evaluate the current product. Additional tree traversal only occurs 'locally' from that operator node.

    Summation has to occur *across* branches of the tree since evaluation of products can generate the same basis element
    in different branches or depths of the tree.

    Some products are anticommutative (exterior and regressive).
    Review the visual below to ensure you understand the 'written'-order versus 'memory'-order of basis elements in each product.

    *Note lhs means left-hand side and rhs means right-hand side.

    When written on paper the product e1 ^ (e0^e2) is comprised of a lhs e1, and rhs e0^e2.
    If we want to write the product as -e0^e1^e2 we swap e1 and e0 and negate the product. 
    Notice that the *right*-most element of the lhs is next to the *left*-most element of the rhs.

    The above doesn't hold from our current implmentation of basis elements being the OR of single bits representing individual basis elements.
    Below is how these elements are juxtaposed against one another in memory.

    In the LHS, the right-most basis element factor (when written) is stored in the most-significant bit.
    In the RHS, the left-most  basis element factor (when written) is stored in the least-significant bit.

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

    simplified.root = scratch_to_ast();
    return simplified;
}

reset_scratch :: () {

    memset(scratch.buffer.data, 0,  Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS * size_of(List(Term)));
    scratch.populated_count = 0;

    return;
}

#scope_file

Complement_Type :: enum {
    Left;
    Right;
}

Scratch :: struct {

    /*
        Scratch space is used to sum the tree nodes after evaluation of all other operations since
        the same basis element can be spread across different branches and depth of the tree.
        
        Each basis element is represented with a unique bitset we can use that as a unique index 
        into the scratch buffer. So we traverse the tree and 'flatten' each basis element into its location in the buffer
        and calculate the sum of coefficients for each basis element remaining in the tree at that index. This generates
        the final coefficient of the basis element.

        Once that's completed the scratch buffer is used to reconstruct the simplified tree back into an AST representation
        that can be stored as a variable's value as part of an assignment and used in subsequent expressions.

    */

    buffer:          [Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS] List(Term);
    populated_slots: [Exterior_Space.MAX_NUMBER_OF_BASIS_ELEMENTS] int;
    populated_count: int;
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
            case .Scalar_Multiplication; { result = scalar_multiplication(lhs, rhs); }
            case .Exterior_Product;      { result = exterior_product(lhs, rhs);      }
            case .Regressive_Product;    { result = regressive_product(lhs, rhs);    }
            case; {
                // Sums are handled in a separate pass over the tree.
                result = binary_op;
            }
        }
    }

    // This function should only return basis element nodes and addition and subtraction operator nodes.
    return result;

}

negate :: (element: *Element_Node) -> *Element_Node {

    assert(element != null);
    
    scale_coefficient(element.coefficient, -1);
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
        scale_coefficient(n_k.coefficient, -1);
    }

    return n_k;
}

scalar_multiplication :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    assert(lhs.basis.grade == 0 || rhs.basis.grade == 0);

    coefficient := new_coefficient(0);
    result      := new_element_node(0, coefficient, get_basis_element(0));

    result.coefficient = multiply_coefficients(lhs.coefficient, rhs.coefficient);
    result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);

    return result;
}

exterior_product :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    coefficient := new_coefficient(0);
    result      := new_element_node(0, coefficient, get_basis_element(0));

    if lhs.basis.grade == 0 || rhs.basis.grade == 0 {
        // scalar multiplication
        result.coefficient = multiply_coefficients(lhs.coefficient, rhs.coefficient);
        result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);
    }
    else
    if (lhs.basis.bitset & rhs.basis.bitset) == 0 {
        // no one elements in common; implies sum of grades is less than or equal to N.
        result.coefficient = multiply_coefficients(lhs.coefficient, rhs.coefficient);
        result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);

        anticommute := does_product_anticommute(lhs.basis.bitset, rhs.basis.bitset);
        if anticommute {
            scale_coefficient(result.coefficient, -1);
        }
    }
    // else no one element in common, result is zero.

    return result;
}

regressive_product :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    coefficient := new_coefficient(0);
    result      := new_element_node(0, coefficient, get_basis_element(0));
    if (lhs.basis.bitset | rhs.basis.bitset) == space.n_element.bitset {

        result.basis = get_basis_element(lhs.basis.bitset & rhs.basis.bitset);
        if result.basis.bitset == 0 {
            // The lhs and rhs are complements of one another generating a zero element.
            result.coefficient = multiply_coefficients(lhs.coefficient, rhs.coefficient);

            anticommute := does_product_anticommute(lhs.basis.bitset, rhs.basis.bitset);
            if anticommute {
                scale_coefficient(result.coefficient, -1);
            }
        }
        else {
            result.coefficient = multiply_coefficients(lhs.coefficient, rhs.coefficient);

            anticommute1 := does_product_anticommute(lhs.basis.bitset ^ result.basis.bitset, result.basis.bitset);
            anticommute2 := does_product_anticommute(result.basis.bitset, rhs.basis.bitset);

            if anticommute1 != anticommute2 {
                scale_coefficient(result.coefficient, -1);
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

        result: List(Term) = ---;

        coefficient := scratch.buffer[element.basis.bitset];
        if zero_terms(coefficient) {
            // first time during the current expression evaluation where we've encountered this basis element in the tree.
            result = copy_terms(element.coefficient);
            scratch.populated_slots[scratch.populated_count] = element.basis.bitset;
            scratch.populated_count += 1;
        }
        else {
            result = combine_like_terms(coefficient, element.coefficient);
        }
        scratch.buffer[element.basis.bitset] = result;
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        assert(binary_op.op == .Addition);

        sum(binary_op.lhs);
        sum(binary_op.rhs);
    }
    
    return;
}

scratch_to_ast :: () -> *Ast_Node {

    bitset:       int   = 0;
    coefficient:  List(Term) = ---;
    basis:       *Basis_Element = null;

    if scratch.populated_count == 1 {
        bitset      = scratch.populated_slots[0];
        coefficient = scratch.buffer[bitset];
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
            coefficient = scratch.buffer[bitset];
            basis       = get_basis_element(bitset);

			current.lhs = new_element_node(0, coefficient, basis);
			next := new_binary_operator_node(0, null, .Addition, null);
			current.rhs = next;
			current      = next;

			i += 1;
		}

        bitset       = scratch.populated_slots[i];
        coefficient  = scratch.buffer[bitset];
        basis        = get_basis_element(bitset);

		current.lhs  = new_element_node(0, coefficient, basis);

        bitset       = scratch.populated_slots[i+1];
        coefficient  = scratch.buffer[bitset];
        basis        = get_basis_element(bitset);

		current.rhs = new_element_node(0, coefficient, basis);

        return root;
	}
}

// Utility functions for Grassmann products that operate on individual bits.
bit_is_set :: inline (bitset: int, bit: int) -> bool {
    return (bitset & (1 << bit)) != 0;
}

set_bit :: inline (bitset: *int, bit: int) {
    bitset.* |= (1 << bit);
}