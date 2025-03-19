/*
    This file expands/distributes Grassmmann products over sums and differences.
    The new expression tree (AST) is evaluated in the 'simplify pass over the AST.

    This step is not conceptually hard but the code is a little terse so here is a visual example
    so I can remember how I did this.

    --------------------------------------------------------------------------------------
    Example: Simple expansion    
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
    Example: Recursive expansion 
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
                        ^
                    +      e3
                e0   e1

                       +
                    ^    ^
                e0  e3  e1 e3

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

                // This procedure can generate another Grassmann product over a sum.
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
            if rhs.kind == .Binary_Operator && (rhs.op == .Addition || rhs.op == .Subtraction) {
                // Distribute the lhs over the rhs
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

                // This procedure can generate another Grassmann product over a sum.
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