/*
    Symbol resolution is the first pass over the AST.
    Its goal is to inspect identifier nodes and determine if they are variables (to be assigned to), basis elements, or scalar variables.
    Also it substitutes AST nodes for already declared and defined variables.

    This pass does *not* validate the semantics of the AST after resolution. The semantic pass occurs after this
    step and reports errors in the usage of identifiers or operators.
*/

#scope_export

resolve_symbols :: (ast: *Ast, space: *Exterior_Space) {

    ast.root = resolve(ast.root, null, space);
    return;
}

#scope_file

resolve :: (node: *Ast_Node, parent: *Ast_Node, space: *Exterior_Space) -> *Ast_Node {

    result := node;
    if node.kind == .Identifier {
        ident := cast(*Identifier_Node)node;

        while true { // poor man's goto!

            _1_element := matches_1_element(ident.name);
		    if _1_element {
                coefficient := new_term(1);
			    element     := new_element_node(ident.node_id, coefficient, _1_element);

                result = element;
                break;
		    }

            match := match_scalar_variable(ident.name);
            if match {
                valid := valid_scalar_variable(ident.name);
                if valid {
                    coefficient := new_term(multiples = 1, exponent = 1, scalar_variable = ident.name );
                    basis       := get_basis_element(0);
                    element     := new_element_node(ident.node_id, coefficient, basis);

                    result = element;
                }
                else {
                    message := tprint("Error, parsed identifier '%' as a scalar variable. Scalar variables contain a leading character from this list '%' followed by a maximum of 2 digits.\n", ident.name, space.scalar_variable_prefix_characters);
                    error   := new_error_node(ident.node_id, message);

                    result = error;
                }
                
                break;
            }

            if parent && parent.kind == .Binary_Operator {
                binary_op := cast(*Binary_Operator_Node)parent;
                if binary_op.op == .Assignment {
                    var := new_variable_node(ident.node_id, ident.name);
                    result = var;

                    break;
                }

                // Do *not* break out of the loop here! Want to check for undeclared identifiers below.
            }

            // This is non-traditional but the 'value' from a previous expression is an M-element (a 'multi-element').
            // Basically a list of simplified sums of disjoint graded elements.
            // Instead of storing it as a list like "e1 + e1^e2 + e1^e2^e3" it is represented as a tree of AST nodes so during
            // this pass over the new AST we can inject this tree in place and keep on moving.
            // The Grassmann products and sums have been simplified as much as possible during the previous expression evaluation
            // so we aren't 'redoing' the same work again but it doesn't make sense to store the multi-element in the symbol table
            // as a string then try to figure out how to parse it again or something stupid like that.
            found, variable := lookup_variable(ident.name);
            if found {
                variable.ast.node_id = ident.node_id;
                result = variable.ast;
                break;
            }
            else {
                message := tprint("Error, undeclared identifier '%'\n", ident.name);
                error   := new_error_node(ident.node_id, message);

                result = error;
                break;
            }
        }
    }

    // Resolve the children of any operators.
    if node.kind == .Unary_Operator {
        unary_op := cast(*Unary_Operator_Node)node;
        unary_op.rhs = resolve(unary_op.rhs, unary_op, space);

        result = unary_op;
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        binary_op.lhs = resolve(binary_op.lhs, binary_op, space);
        binary_op.rhs = resolve(binary_op.rhs, binary_op, space);

        result = binary_op;
    }

    return result;
}