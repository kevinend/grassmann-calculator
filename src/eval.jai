/*
    Evaluation/simplification of the AST occurs in two steps.
        1. Evaluate all "local" operations.
        2. Evaluate addition/summation.

    Products and unary operators are 'local' operations. While traversing the tree you can evaluate their children
    then evaluate the current product operation. Additional tree traversal only occurs 'locally' from that operator node.

    Summation has to occur *across* branches of the tree since evaluation of products can generate the same basis term
    in different branches or depths of the tree.

    Some products are anticommutative (exterior and regressive).
    Review the visual below to ensure you understand the 'written'-order versus 'memory'-order of basis elements in each product.

    *Note lhs means left-hand side and rhs means right-hand side.

    When written on paper the product e1 ^ (e0^e2) is comprised of a lhs e1, and rhs e0^e2.
    If we want to write the product as -e0^e1^e2 we swap e1 and e0 and negate the product. 
    Notice that the *right*-most element of the lhs is next to the *left*-most element of the rhs visually.

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

/*
simplify_and_assign :: (ast: Ast, user_variables: *Table(string, *Ast_Node), scratch: *Scratch) {

    reset_scratch_space(scratch);

    assignment_node := cast(*Binary_Operator_Node)ast.root;

    variable        := cast(*Variable_Node)assignment_node.lhs;
    expression      := assignment_node.rhs;

    eval_context := context;
    eval_context.allocator = temp;
    push_context eval_context {
        partial_result := eval(expression);
        sum(partial_result, scratch);
    }

    // AST assigned to the variable is allocated in the heap.
    value := scratch_to_ast(scratch);
    update_or_insert_variables(variable.name, value, user_variables);

    return;
}
*/

simplify :: (ast: Ast, working_set: *Working_Set) -> Ast {

    output: Ast;
    
    reset_working_set(working_set);

    expression := ast.root;
    eval(expression, null, working_set);

    ast_generation_context := context;
    ast_generation_context.allocator = temp;
    push_context ast_generation_context {
//        output = working_set_to_ast(working_set);
    }
    
    return output;
}

#scope_file

// Utility functions for Grassmann products that operate on individual bits.
bit_is_set :: inline (bitset: int, bit: int) -> bool {
    return (bitset & (1 << bit)) != 0;
}

set_bit :: inline (bitset: *int, bit: int) {
    bitset.* |= (1 << bit);
}

eval :: (node: *Ast_Node, parent: *Ast_Node, working_set: *Working_Set) -> *Ast_Node {

    basis_term_node: *Basis_Term_Node = null;
    if node.kind == .Basis_Term {
        basis_term_node = cast(*Basis_Term_Node)node;
    }

    if parent == null {
        // could argue we conceptually sum the coefficients or that we sum basis terms 3e1 + 4e1, we are summing the basis terms
        // so we should probably do the following:
        //   store_basis_term()
        //   load_basis_term(working_set, )
        bitset := get_basis_bitset(basis_term_node.basis_term);
        lhs    := store_coefficient(working_set, basis_term_node.coefficient);
        rhs    := load_coefficient(working_set, bitset); // TODO: this should be the .'basis' not the bitset

        result := sum(lhs, rhs);

       // update_coefficient(working_set, bitset, result);
    }

    return basis_term_node;
}
      

/*
 Starting over with eval on 04.26.2025

eval :: (node: *Ast_Node, parent: *Ast_Node, working_set: *Working_Set) {

    if node.kind == .Basis_Term {
        current = cast(*Basis_Term_Node)node;
        current.coefficient = load_working_set_coefficient(working_set, current.coefficient);
    }

    // This is the processing part!
    if parent == .null || parent == .Addition {
        basis := result.element.basis;
        lhs   := result.coefficient;

        rhs   := get_basis_coefficient(working_set, basis);

        results := get_next_buffer(working_set);
        sum     := add_coefficients(lhs, rhs, results);
        update_basis_coefficient(working_set, basis, sum);

        reset_buffers(working_set);
    }
    else {
        return result;
    }

    return current;

/* Trust no one!
    else
    if node.kind == .Unary_Operator {
        unary_op := cast(*Unary_Operator_Node)node;
        unary_op.rhs = eval(unary_op.rhs, unary_op, working_set);

        assert(unary_op.rhs.kind == .Basis_Term);
        rhs := cast(*Basis_Term_Node)unary_op.rhs;

        output := prepare_buffer(working_set);

        if unary_op.op == {
            case .Negation;         { negate    (              rhs.basis_term, *output); }
            case .Left_Complement;  { complement(left = true,  rhs.basis_term, *output); }
            case .Right_Complement; { complement(left = false, rhs.basis_term, *output); }
        }

        result_node = rhs;
        result_node.basis_term = output;
    }

    result: *Element_Node = null;
    if node.kind == .Element {
        element := cast(*Element_Node)node;
        

        rotate_active_scratch_buffer(working_set);
        element.coefficient = load_coefficient_into_working_set(working_set, element.coefficient);

        result = element;
    }
    else
    if node.kind == .Unary_Operator {
        unary_op    := cast(*Unary_Operator)node;
        unary_op.rhs = eval(unary_op.rhs, unary_op);

        assert(unary_op.rhs == .Element);
        element := cast(*Element_Node)unary_op.rhs;
    
        if unary_op.op == {
            case .Negation;         { result = negate(element); }
            case .Left_Complement;  { result = complement(Complement_Type.Left,  element); }
            case .Right_Complement; { result = complement(Complement_Type.Right, element); }
        }
    }
    else
    if node.kind == .Associative_Operator {
        associative_op := cast(*Associative_Operator)node;
        args := associative_op.args;

        if associative_op.op == .Addition {
            // Addition is deferred until down below.
            for i: 0..args.count-1 { eval(args[i]); }
        }
        else {
            lhs := eval(args[0]);
            for i: 1..args.count-1 {
                rhs := eval(args[i]);

                rotate_active_scratch_buffer(working_set);
                intermediate := get_scratch_cofficient(working_set); // could we load zero in here?

                if associative_op.op == {
                    case .Exterior_Product;    { result = exterior_product  (lhs, rhs, intermediate); }
                    case .Regressive_Product;  { result = regressive_product(lhs, rhs, intermediate); }
                }
                lhs = result;
            }
        }
    }

    if parent == .null || parent == .Addition {
        basis := result.element.basis;
        lhs   := result.coefficient;

        rhs   := get_basis_coefficient(working_set, basis);

        results := get_next_buffer(working_set);
        sum     := add_coefficients(lhs, rhs, results);
        update_basis_coefficient(working_set, basis, sum);

        reset_buffers(working_set);
    }
    else {
        return result;
    }

*/ 
}

*/


/*

// TODO: 04.12.2025
// result := load_element_into_working_set(working_set, element);
// Like you loaded the thing into the working set
// It is a pointer to a basis element, so you can have a basis element, load it up with the data
// Then copy the coefficient into the coefficient data section and return a pointer to this element
//
// How does this help with the binary operator case?
// We could have the product load zero?
//  result = load_element_into_working_set(working_set, zero);
// that is what we do in both products anyways and the result is typically zero so we don't have to do anything
// it wouldn't change. Cool, that might be a play there!

eval :: (node: *Ast_Node, parent: *Ast_Node, working_set: *Working_Set) {

    result: *Element_Node = null;
    if node.kind == .Element {
        element := cast(*Element_Node)node;

        rotate_active_scratch_buffer(working_set);
        element.coefficient = load_coefficient_into_working_set(working_set, element.coefficient);

        result = element;
    }
    else
    if node.kind == .Unary_Operator {
        unary_op    := cast(*Unary_Operator)node;
        unary_op.rhs = eval(unary_op.rhs, unary_op);

        assert(unary_op.rhs == .Element);
        element := cast(*Element_Node)unary_op.rhs;
    
        if unary_op.op == {
            case .Negation;         { result = negate(element); }
            case .Left_Complement;  { result = complement(Complement_Type.Left,  element); }
            case .Right_Complement; { result = complement(Complement_Type.Right, element); }
        }
    }
    else
    if node.kind == .Associative_Operator {
        associative_op := cast(*Associative_Operator)node;
        args := associative_op.args;

        if associative_op.op == .Addition {
            // Addition is deferred until down below.
            for i: 0..args.count-1 { eval(args[i]); }
        }
        else {
            lhs := eval(args[0]);
            for i: 1..args.count-1 {
                rhs := eval(args[i]);

                rotate_active_scratch_buffer(working_set);
                intermediate := get_scratch_cofficient(working_set); // could we load zero in here?

                if associative_op.op == {
                    case .Exterior_Product;    { result = exterior_product  (lhs, rhs, intermediate); }
                    case .Regressive_Product;  { result = regressive_product(lhs, rhs, intermediate); }
                }
                lhs = result;
            }
        }
    }

    if parent == .null || parent == .Addition {
        basis := result.element.basis;
        lhs   := result.coefficient;

        rhs   := get_basis_coefficient(working_set, basis);

        results := get_next_buffer(working_set);
        sum     := add_coefficients(lhs, rhs, results);
        update_basis_coefficient(working_set, basis, sum);

        reset_buffers(working_set);
    }
    else {
        return result;
    }

}

/* TODO: 04.12.2025 -- Remove this once our new eval is working!
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

*/

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

    coefficient := zero();
    result      := new_element_node(0, coefficient, get_basis_element(0));

    result.coefficient = multiply_coefficients(lhs.coefficient, rhs.coefficient);
    result.basis       = get_basis_element(lhs.basis.bitset | rhs.basis.bitset);

    return result;
}

exterior_product :: (lhs: *Element_Node, rhs: *Element_Node) -> *Element_Node {

    coefficient := zero();
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

    coefficient := zero();
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

sum :: (node: *Ast_Node, scratch: *Scratch) {

    if node.kind == .Element {
        element := cast(*Element_Node)node;

        lhs := load_coefficient(scratch, element.basis.bitset);
        rhs := store_then_load_coefficient(*scratch.working_set, element.coefficient);

        if number_of_terms(lhs) == 0 {
            sum_coefficients(lhs, rhs);
            mark_coefficient_populated(scratch, element.basis.bitset);
        }
        else {
            sum_coefficients(lhs, rhs);
        }
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        assert(binary_op.op == .Addition);

        sum(binary_op.lhs, scratch);
        sum(binary_op.rhs, scratch);
    }


    /* This is the previous sum, leaving for now!

        // search the AST for basis elements and add each into it's location in the scratch buffer.
        if node.kind == .Element {
            element := cast(*Element_Node)node;

            result: Coefficient;

            coefficient := scratch.buffer[element.basis.bitset];
            if coefficient_contains_no_terms(coefficient) {
                // first time during the current expression evaluation where we've encountered this basis element in the tree.
                result = deep_copy_terms(element.coefficient);
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

            sum(binary_op.lhs, scratch);
            sum(binary_op.rhs, scratch);
        }

    */
    

    
    return;
}



*/