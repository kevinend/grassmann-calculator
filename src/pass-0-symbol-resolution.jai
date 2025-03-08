/*
    The purpose of symbol resolution is to construct symbol table entries for new symbol nodes and substitute
    existing symbol's into the AST. The only node types that need substituted are those used in the expression context
    (that is not on the lhs of an assignment operator).
    
    This pass does *not* validate the semantics of the AST after resolution. The semantic pass occurs after this
    step and reports errors in the usage of symbols or operators.
*/

#scope_export

resolve_symbols :: (ast: *Ast, symbol_table: *Symbol_Table) {

    ast.root = resolve(ast.root, symbol_table);
    return;
}

#scope_file

resolve :: (node: *Ast_Node, symbol_table: *Symbol_Table) -> *Ast_Node {

    result := node;
    if node.kind == .Symbol {
        // resolve the symbol if it used in an expression context, that is that it's parent is not the assignment operator!
        result = node;
    }

    // Resolve the children of the operators.
    if node.kind == .Unary_Operator {
        unary_op := cast(*Unary_Operator_Node)node;
        unary_op.rhs = resolve(unary_op.rhs, symbol_table);

        result = unary_op;
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        binary_op.lhs = resolve(binary_op.lhs, symbol_table);
        binary_op.rhs = resolve(binary_op.rhs, symbol_table);

        result = binary_op;
    }

    return result;
}