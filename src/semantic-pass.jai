/*
    This file implements a semantic pass over the AST.
    Checks the AST to make sure what the user input makes sense with respect to the algebra.
*/

#scope_export

semantic_check :: (ast: *Ast) -> bool {

    // check
    // report

    pass: bool = true;
    report(ast.root, ast.position_metadata, *pass);
    return pass;
}

#scope_file

report :: (node: *Ast_Node, position_metadata: [] Position_Info, pass: *bool) {

    if node.kind == .Error {
        error    := cast(*Error_Node)node;
        position := position_metadata[error.node_id];
        print("Error! Line:%, Char:%  %\n", position.line_number, position.char_start, error.message);
        pass.* = false;
    }

    if node.kind == .Unary_Operator {
        unary_op    := cast(*Unary_Operator_Node)node;
        report(unary_op.rhs, position_metadata, pass);
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;
        report(binary_op.lhs, position_metadata, pass);
        report(binary_op.rhs, position_metadata, pass);
    }

    return;
}