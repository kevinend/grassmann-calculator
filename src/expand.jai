/*
    This file expands/distributes Grassmmann products over sums and differences.
    The new expression tree (AST) is evaluated in the 'simplify pass over the AST.

    This step is not conceptually hard but the code is a little terse so here is a visual example
    so I can remember how I did this.

    --------------------------------------------------------------------------------------
    Unary Operator Example:
    !(e0 + e1 + e2)

    Initial Tree: 
                ! (Left-complement)
                   +
                e0   +
                    e1  e2
    
    New subtree before recursive expansion.
                +
             !     !
            e0        +
                    e1   e2

    Final subtree post recursive expansion.
                +
            !       +
            e0    !    !
                  e1   e2

    
    --------------------------------------------------------------------------------------
    Binary Operator Example: Simple expansion    
    e0 ^ (e1 + e2)

    Initial tree:
                        ^
                    e0     +
                        e1   e2

    New LHS and RHS Nodes:
        LHS:  ^       RHS:   ^
            e0 e1          e0 e2

    New subtree with new LHS and RHS:
                 +
              ^     ^
            e0 e1 e0 e2
    --------------------------------------------------------------------------------------
    Binary Operator Example: Recursive expansion 
    (e0 + e1) ^ (e2 + e3)

    Initial tree:           ^
                        +        +
                    e0   e1  e2  e3

    New LHS and RHS:
        LHS      ^          RHS  ^
              e0    +          e1  +
                 e2   e3          e2 e3

    New subtree with new LHS and RHS (without the recursive expansion/distribution):
                        +
                    ^       ^
                e0    +   e1   +
                    e2 e3    e2 e3


    Need to invoke' distribute' on the new LHS and RHS so they also get distributed over the sum/difference.

    Fully distributed/expanded subtree:
                            +
                    +                  +
                ^       ^         ^         ^
            e0   e2  e0   e3   e1  e2    e1  e3
    --------------------------------------------------------------------------------------
    
*/

#scope_export

expand :: (ast: Ast) -> Ast {

    expanded_ast: Ast;
    expanded_ast.root = distribute(ast.root);
    
    return expanded_ast;
}

#scope_file

distribute :: (node: *Ast_Node) -> *Ast_Node {

    // Distribute the grassmann products over the arithmetic sum or difference.

    subtree: *Ast_Node;
    if node.kind == .Unary_Operator {
        unary_op := cast(*Unary_Operator_Node)node;
    
        // Unary operators do *not* need distributed over Grassmann products.
        // complement(e1^e2) will be a complement operation over the exterior product of e1 and e2, it should not distribute.

        // *assume* the rhs of the current operator is a binary operator. Saves some nesting.
        binary_op := cast(*Binary_Operator_Node)unary_op.rhs;
        if unary_op.rhs.kind == .Binary_Operator && binary_op.op == .Addition {
            lhs := new_unary_operator_node(unary_op.node_id, unary_op.op, copy_node(binary_op.lhs));
            rhs := new_unary_operator_node(unary_op.node_id, unary_op.op, copy_node(binary_op.rhs));

            subtree = new_binary_operator_node(
                 binary_op.node_id
                ,distribute(lhs)
                ,binary_op.op
                ,distribute(rhs)
            );
        }
        else {
            subtree = copy_node(unary_op);
        }
    }
    else
    if node.kind == .Binary_Operator {
        binary_op := cast(*Binary_Operator_Node)node;

        if binary_op.op == .Exterior_Product || binary_op.op == .Regressive_Product || binary_op.op == .Scalar_Multiplication {
            product_op := binary_op;

            // *assume* the lhs and rhs of the current operator are binary operators.
            // this saves us some nesting and from having to instantiate these vars later if they *do* turn out to be operators.
            lhs := cast(*Binary_Operator_Node)product_op.lhs;
            rhs := cast(*Binary_Operator_Node)product_op.rhs;

            // check if the Grassmann products are over sums or differences.
            if lhs.kind == .Binary_Operator && lhs.op == .Addition {

                 new_lhs := new_binary_operator_node(
                     product_op.node_id
                    ,copy_node(lhs.lhs)
                    ,product_op.op
                    ,copy_node(product_op.rhs)
                );

                new_rhs := new_binary_operator_node(
                     product_op.node_id
                    ,copy_node(lhs.rhs)
                    ,product_op.op
                    ,copy_node(product_op.rhs)
                );

                // This procedure can generate another product over a sum.
                // To fully expand the subtrees (and distribute appropriately) need to recursively
                // call this procedure on the new children of the new binary operator.
                // Example (e0+e1)^(e2+e3) will expand to e0|(e2+e3) + e1|(e2+e3) in this invocation
                // which needs expanded again, hence the recursive call.
                subtree = new_binary_operator_node(
                     lhs.node_id
                    ,distribute(new_lhs)
                    ,lhs.op
                    ,distribute(new_rhs)
                );
            }
            else
            if rhs.kind == .Binary_Operator && rhs.op == .Addition {
                // Distribute the lhs over the rhs
                // See above comment since this is the same thing but from the other branch of the tree.

                new_lhs := new_binary_operator_node(
                     product_op.node_id
                    ,copy_node(product_op.lhs)
                    ,product_op.op
                    ,copy_node(rhs.lhs)
                );

                new_rhs := new_binary_operator_node(
                     product_op.node_id
                    ,copy_node(product_op.lhs)
                    ,product_op.op
                    ,copy_node(rhs.rhs)
                );

                // This procedure can generate another product over a sum.
                // To fully expand the subtrees (and distribute appropriately) need to recursively
                // call this procedure on the new children of the new binary operator.
                // Example (e0+e1)^(e2+e3) will expand to e0|(e2+e3) + e1|(e2+e3) in this invocation
                // which needs expanded again, hence the recursive call.
                subtree = new_binary_operator_node(
                     rhs.node_id
                    ,distribute(new_lhs)
                    ,rhs.op
                    ,distribute(new_rhs)
                );
                
            }
            else {
                subtree = copy_node(product_op);
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