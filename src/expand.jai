/*
    This file expands/distributes Grassmmann products over sums.
    Example: e1 ^ (e2 + e3) == e1^e2 + e1^e3
    
    The new expressions are evaluated in then 'simplify' pass over the AST.
*/

#scope_export

expand :: (ast: Ast) -> Ast {

    expanded_ast: Ast;
    expanded_ast.root = distribute(ast.root);
    
    return expanded_ast;
}

#scope_file

distribute :: (node: *Ast_Node) -> *Ast_Node {

    // Distribute the grassmann products over the arithmetic product (sum)
	// 
	// This procedure generates a whole new expression tree based on the initial expression tree
	// supplied by the caller. Ideally, we can update the tree in place, which is totally do-able
	// but until I have an initial implmentation up and working I think it is best to have
	// the 'expansion' or 'simplification' stages work on their own trees to ease debugging.

    subtree: *Ast_Node;
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;

        if binary_op.op == .Exterior_Product || binary_op.op == .Regressive_Product {
            grassmann_op := binary_op;

            // *assume* the lhs and rhs of the current operator are binary operators.
            // this saves us some nesting and from having to instantiate these vars later if they *do* turn out to be operators.
            lhs := cast(*Binary_Operator_Node)grassmann_op.lhs;
            rhs := cast(*Binary_Operator_Node)grassmann_op.rhs;

            // check if the Grassmann products are over sums or differences.
            if lhs.kind == .Binary_Operator && (lhs.op == .Addition || lhs.op == .Subtraction) {
                /*
                    Distribute the lhs over the rhs
                    Example - Simple expansion:    (e1 + e2) ^ e3
                    Example - Recursive expansion: (e1 + e2) ^ (e3 + e4)

                    Initial tree:           ^
                                       +        +
                                    e1   e2  e3  e4
                
                    New LHS and RHS:
                        LHS      ^          RHS  ^
                              e1  +            e2  +
                                e3 e4            e3 e4
                
                
                    Need to invoked distribute on the new LHS and RHS so they also get distributed.

                    Expanded subtree:
                                            +
                                    +                  +
                                ^       ^         ^         ^
                            e1   e3  e1   e4    e2  e3    e2  e4
                */
                         
                new_lhs := new_binary_operator_node(
                     grassmann_op.node_id
                    ,copy_node(lhs.lhs)
                    ,grassmann_op.op
                    ,copy_node(grassmann_op.rhs)
                );

                new_rhs := new_binary_operator_node(
                     grassmann_op.node_id
                    ,copy_node(lhs.rhs)
                    ,grassmann_op.op
                    ,copy_node(grassmann_op.rhs)
                );

                // to fully expand the subtrees, need to recursively call this procedure again
                subtree = new_binary_operator_node(
                     lhs.node_id
                    ,distribute(new_lhs)
                    ,lhs.op
                    ,distribute(new_rhs)
                );
            }
            else
            if rhs.kind == .Binary_Operator && (rhs.op == .Addition || rhs.op == .Subtraction) {
                // Distribute the rhs over the lhs
                // See above comment since this is the same thing but from the other branch of the tree.
                
                 new_lhs := new_binary_operator_node(
                     grassmann_op.node_id
                    ,copy_node(grassmann_op.lhs)
                    ,grassmann_op.op
                    ,copy_node(rhs.lhs)
                );

                new_rhs := new_binary_operator_node(
                     grassmann_op.node_id
                    ,copy_node(grassmann_op.lhs)
                    ,grassmann_op.op
                    ,copy_node(rhs.rhs)
                );

                // to fully expand the subtrees, need to recursively call this procedure again
                subtree = new_binary_operator_node(
                     rhs.node_id
                    ,distribute(new_lhs)
                    ,rhs.op
                    ,distribute(new_rhs)
                );
            }
            else {
                subtree = copy_node(grassmann_op);
            }
        }
        else {
            subtree = copy_node(binary_op);
        }
    }
    else {
        subtree = copy_node(node);
    }

    assert(subtree != null);
    return subtree;
}