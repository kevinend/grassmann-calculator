/*
    Symbol resolution is the first pass over the AST.
    Its goal is to inspect identifier nodes and determine if they are variables (lhs of assignments), basis elements, or scalar variables.
    Also it substitutes AST nodes for already declared and defined variables.

    This pass does *not* validate the semantics of the AST after resolution.
    It does generate recognize errors during resolution like undeclared identifiers or malformed scalar variables.
    These error nodes are added in place of the identifier and will be reported during the semantic pass.
*/

#scope_export

resolve_symbols :: (ast: *Ast, space: *Exterior_Space) {

    assert(ast      != null);
    assert(ast.root != null);

    ast.root = resolve(ast.root, null, space);
    return;
}

#scope_file

resolve :: (node: *Ast_Node, parent: *Ast_Node, space: *Exterior_Space) -> *Ast_Node {

    if node.kind == .Identifier {

        ident := cast(*Identifier_Node)node;

        // Check if identifier matches one of the currently declared basis 1-elements.

        _1_element := matches_1_element(ident.name);
        if _1_element { 
            coefficient := new_term(1);
            element     := new_element_node(ident.node_id, coefficient, _1_element);

            return element;
        }

        // Check if identifier starts with a character of the currently declared scalar variables. If so, validate it matches the schema otherwise return an error.

        match := match_scalar_variable(ident.name);
        if match {
            valid := valid_scalar_variable(ident.name);
            if valid {
                coefficient := new_term(multiples = 1, exponent = 1, scalar_variable = ident.name );
                basis       := get_basis_element(0);
                element     := new_element_node(ident.node_id, coefficient, basis);

                return element;
            }
            else {
                message := tprint("Parsed identifier '%' as a scalar variable. Scalar variables contain a leading character from the list '%' followed by a maximum of 2 digits.", ident.name, space.scalar_variable_prefix_characters);
                error   := new_error_node(ident.node_id, message);

                return error;
            }
        }

        // Check if the identifier is the left-hand side of an assignment operator.

        if parent && parent.kind == .Binary_Operator {
            binary_op := cast(*Binary_Operator_Node)parent;
            if binary_op.op == .Assignment && binary_op.lhs == node {
                var := new_variable_node(ident.node_id, ident.name);
                return var;
            }

            // Do *not* return here! Want to check for undeclared identifiers below.
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
            return variable.ast;
        }
        else {
            message := tprint("Undeclared identifier '%'.", ident.name);
            error   := new_error_node(ident.node_id, message);

            return error;
        }
    }

    // Resolve the children of any operators.
    if node.kind == .Unary_Operator {
        unary_op := cast(*Unary_Operator_Node)node;
        unary_op.rhs = resolve(unary_op.rhs, unary_op, space);

        return unary_op;
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        binary_op.lhs = resolve(binary_op.lhs, binary_op, space);
        binary_op.rhs = resolve(binary_op.rhs, binary_op, space);

        return binary_op;
    }

    return node;
}