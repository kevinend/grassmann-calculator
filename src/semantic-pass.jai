/*
    This file implements a semantic pass over the AST.
    Errors in the leaves are pushed up through the tree to the root.
    I *think* this is called 'poisoning'. Instead of having operators generate nonsense errors because the children
    are malformed just bubble those errors to the root and report the issue.

    Current checks:
    1. Undeclared identifiers (error nodes generated during symbol resolution)
    2. Variables are the only targets for assignment (no basis elements or scalars).
    3. Function calls *must* use intrinsic's (no user-defined functions).
*/

#scope_export

check_semantics :: (ast: *Ast) -> bool {

    passed: bool = true;

    ast.root = check(ast.root);
    if ast.root.kind == .Error {
        error := cast(*Error_Node)ast.root;
        print("%\n", error.message);

        passed = false;
    }

    return passed;
}

#scope_file

check :: (node: *Ast_Node) -> *Ast_Node {

    if node.kind == {
        case .Unary_Operator; {
            unary_op := cast(*Unary_Operator_Node)node;
            rhs      := check(unary_op.rhs);

            if rhs.kind == .Error { return rhs; }
        }
        case .Binary_Operator; {
            binary_op := cast(*Binary_Operator_Node)node;
            lhs := check(binary_op.lhs);
            rhs := check(binary_op.rhs);

            if lhs.kind == .Error { return lhs; }
            if rhs.kind == .Error { return rhs; }

            if binary_op.op == .Assignment && lhs.kind == .Element {
                element := cast(*Element_Node)lhs;
                message: string = ---;
                if element.basis.grade == 0 {
                    message = tprint("Scalar cannot be the target of an assignment.");
                }
                else {
                    message = tprint("Basis element cannot be the target of an assignment.");
                }

                error := new_error_node(element.node_id, message);
                return error;
            }

            if binary_op.op == .Function_Call && lhs.kind == .Element {
                element := cast(*Element_Node)lhs;
                message: string = ---;
                if element.basis.grade == 0 {
                    message = tprint("Juxtaposition for scalar multiplication is not supported, please use the scalar multiplication or exterior product operators between the scalar and the grouping.");
                }
                else {
                    message = tprint("Juxtaposition with a basis element is not supported. Please use a Grassmann operator between the element and the grouping.");
                }

                error := new_error_node(element.node_id, message);
                return error;
            }

            return binary_op;
        }
    }

    return node;
}