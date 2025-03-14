/*
    The parser is based off this algorithm https://github.com/erikeidt/erikeidt.github.io/blob/master/The-Double-E-Method.md.
    The first implementation was a hand-rolled recursive descent parser then a pratt parser and a combo of
    a shunting yard to convert expressions to RPN then recursive descent. The chosen method leverages two stacks so the recursion
    of normal algorithms is replaced with a simple top level loop.

    On Error Reporting:

    Error messages are handled in two phases. Phase 1 occurs parsing where obvious syntatic mistakes have been made.
    Examples include unmatched parenthesis or two identifiers next to each other without an infix operator, etc. Phase 2
    is a pass over the constructed AST to investigate semantic errors. These are things like performing scalar operations
    between two basis elements (which is not defined), assigning an expression to a scalar symbol, or making a function
    call with an unrecognized intrinsic, undefined symbols (using a symbol prior to it's definition), etc..
*/

#scope_export

Parser :: struct {

    tokens: [] Token;
    current_token: int;

    mode:       Mode;
    operators:  Stack(Operator);
    operands:   Stack(*Ast_Node); 
}

/*

Globals!

 Each token tracks the location/position of the text that it was generated from.
 Each AST node should contain this same information to generate decent error messages.
 Carrying around the position information after the semantic pass bloats the AST Nodes and the problem gets worse
 if you clone the nodes during a distribute operation.

 Instead the AST Nodes contain a 'node-id' which is used as an index into a 'position metadata' list containing the position info
 from the token. The node id obviously takes up space but 2 or 4 bytes (depending on how this is implemented) is smaller than
 the current size of the position info which is 24 bytes.

 The AST for the current execution takes a slice of the position global for read-access. It *must* only be written to from within the parser.

 Also these are globals since I didn't realize to do this until the symbol resolution and semantic pass steps were implemented.
 If the parser was rewritten these can probably end up in the parser struct and that should just be passed around to each function as well.
 
*/

node_id: u16;
position_metadata:  [..] Position_Info;

parse :: (parser: *Parser, tokens: [] Token) -> Ast {
    
    ast: Ast;
    reset_parser(parser, tokens);

    parser_context := context;
    parser_context.allocator = temp;
    push_context parser_context {
        ast = parse_expression(parser);
    }
    
    return ast;
}

descend :: (node: *Ast_Node, indent: int = 0) {

    // Utility function to print AST's until the interpreter is up and working!
    if node == null {
        return;
    }

    for i: 0..indent-1 {
		print( " " );
	}

    if node.kind == .Error {
        error := cast(*Error_Node)node;
        print("(%) %", error.node_id, error.message);
    }
    else
    if node.kind == .Unary_Operator {
        operator_node := cast(*Unary_Operator_Node)node;
        print("(%) Unary operator == %\n", operator_node.node_id, operator_node.op);
        descend(operator_node.rhs, indent + 2);
    }
    else
    if node.kind == .Identifier {
        identifier := cast(*Identifier_Node)node;
        print("(%) Identifier == %\n", identifier.node_id, identifier.name);
    }
    else
    if node.kind == .Variable {
        var := cast(*Variable_Node)node;
        print("(%) Variable == %\n", var.node_id, var.name);
    }
    else
    if node.kind == .Element {
        element := cast(*Element_Node)node;
        print( 
			"(%) Element (Coefficient=%, Grade=%, Symbol=%, Bits=%)\n"
            ,element.node_id
            ,coefficient_to_string(element.coefficient)
            ,element.basis.grade
            ,element.basis.symbol
            ,element.basis.bits
		);
    }
    else
    if node.kind == .Binary_Operator {
        binary_operator := cast(*Binary_Operator_Node)node;
        print("(%) Binary Operator == %\n", binary_operator.node_id, binary_operator.op );
        descend(binary_operator.lhs, indent + 2);
        descend(binary_operator.rhs, indent + 2);
    }
    else
    if node.kind == .Intrinsic {
        intrinsic := cast(*Intrinsic_Node)node;
        if intrinsic.type == {
            case .Direction; { print("(%) Intrinsic: Direction\n", intrinsic.node_id); }
            case .Position;  { print("(%) Intrinsic: Position\n",  intrinsic.node_id); }
            case .Basis;     { print("(%) Intrinsic: Basis\n",     intrinsic.node_id); }
        }
    }
}

#scope_file

Mode :: enum u8 {
    Prefix;
    Infix_or_Postfix;
}

reset_parser :: (parser: *Parser, tokens: [] Token) {

    parser.tokens = tokens;
    parser.current_token = 0;

    parser.mode = .Prefix;

    // Globals
    node_id = 0;
    array_reset(*position_metadata);
}

parse_expression :: (parser: *Parser) -> Ast {

    using parser; // short-hand so we don't have to say parser.field all the time.

    while true {
        token := consume_token(parser);
        if token.type == .Semicolon { break; }
        
        if parser.mode == .Prefix {
            op := token_to_prefix_operator[token.type];
            if op.type {
                op.position = token.position;
                stack_push(*operators, op);
            }
            else {
                new_operand(token, *operands);
                parser.mode = .Infix_or_Postfix;
            }
        }
        else
        if parser.mode == .Infix_or_Postfix {
            op := token_to_infix_or_postfix_operator[token.type];
            op.position = token.position;
            if !op.type { 
                // so this would be "identifier identifier"
                // we also want to catch like "identifier operator" that isn't postfix for some reason "e1-" crashed the stack. need to investigate
                print("Error!\n"); 
                continue; 
            }

            if op.type == .Right_Complement {
                // Post-fix operator that needs to stay infix or postfix mode since not expecting another prefix operator or operand.
                apply_operators_with_greater_precedence_then_push(op, *operators, *operands);
                continue;
            }
            
            if op.type == .Group_End {
                apply_operators_until_group_start(*operators, *operands);
            }
            else {
                // Apply operators with higher precedence than our current operator.
                // Once complete the operator stack is either empty or our operator has a higher precedence then the current head of the stack.
                // In either case, make our operator the new head of the operators stack.
                apply_operators_with_greater_precedence_then_push(op, *operators, *operands);     
                parser.mode = .Prefix;
            }
        }
    }

    ast: Ast;
    success, root := apply_remaining_operators(*operators, *operands);
    if success {
        ast.root = root;
        ast.position_metadata = position_metadata;
    }
    else {
        print("Parsing failed!\n");
        ast.root = null;
    }
    
    return ast;
}

peek_token :: (parser: *Parser) -> Token {
    token := parser.tokens[parser.current_token];
    return token;
}

consume_token :: (parser: *Parser) -> Token {
    token := parser.tokens[parser.current_token];
    parser.current_token += 1;
    return token;
}

// There are two ways to generate a new operand on the operands stack.
//  (1) Parse a token that is the left or right-hand side of an operator such as an identifier, basis element, scalar, literal, etc.
//  (2) Applying an operator to operands within the stack generating a new operand node.
new_operand :: (token: Token, operands: *Stack(*Ast_Node)) {

    node: *Ast_Node = null;
    array_add(*position_metadata, token.position);

    if token.type == .Number {
        coefficient := new_term(token.value);
        basis       := get_basis_element(0);

        node = new_element_node(node_id, coefficient, basis);
    }
    else
    if token.type == .Identifier {
        node = new_identifier_node(node_id, token.text);
    }
    else
    if token.type == .Keyword_Direction {
        node = new_intrinsic_node(node_id, .Direction);
    }
    else
    if token.type == .Keyword_Position {
        node = new_intrinsic_node(node_id, .Position);
    }
    else
    if token.type == .Keyword_Basis {
        node = new_intrinsic_node(node_id, .Basis);
    }

    stack_push(operands, node);

    node_id += 1;
    return;
}

new_operand :: (op: Operator, operands: *Stack(*Ast_Node)) {

    // Build the operator AST node using the required number of operands and push the constructed node on the operand stack..

    node: *Ast_Node = null;
    array_add(*position_metadata, op.position);

    if operator_arity[op.type] == {
        case 1; {
            rhs := stack_pop(operands);
            node = new_unary_operator_node(node_id, op.type, rhs);
        }
        case 2; {
            rhs := stack_pop(operands);
            lhs := stack_pop(operands);
            node = new_binary_operator_node(node_id, lhs, op.type, rhs);
        }
        case; {
            message := tprint("Unable to construct operator node, unknown arity for operator %\n", op.type);
            node   := new_error_node(node_id, message);
        }
    }

    stack_push(operands, node);
    
    node_id += 1;
    return;          
}

apply_operators_until_group_start :: (operators: *Stack(Operator), operands: *Stack(*Ast_Node)) {

    // Build new operand nodes with the operator at the head of the stack until we reach either .Group_Start or .Function_Call.
    // If we reach the end of the stack before finding a match then the expression has an unmatched open parenthesis.

    while !stack_is_empty(operators) {
        op := stack_pop(operators);
        if op.type == {
            case .Group_Start; { return; }
            case .Function_Call; {
                // Function call can either be of the form 'functionName(arg1)' or 'functionName(arg1,arg2,arg3,..)
                // In each case the Function Call is a binary operator whose lhs is the function name and the rhs depends on the cases above.
                //  (1) Single arg will have the rhs of the binary operator node for the function call be 'arg1'
                //  (2) Multi-arg  will have the rhs of the binary operator node for the function call be another binary operator of type 'Function_Argument_Separator'
                //      whose lhs and rhs are either the args or another 'Function_Argument_Separator' containing more arguments of the function. Rinse and repeat for all the args in the function call.
                new_operand(op, operands);
                return;
            }
        }

        new_operand(op, operands);
    }

    print("Unmatched closing parenthesis in expression\n");
    return;
}

apply_operators_with_greater_precedence_then_push :: (op: Operator, operators: *Stack(Operator), operands: *Stack(*Ast_Node)) {

    // Build new operand nodes using the operator at the head of the stack while it has a higher precedence than the currently parsed operator.

    op_precedence := operator_precedence[op.type];
    while !stack_is_empty(operators) {
        head_op            := stack_peek(operators);
        head_op_precedence := operator_precedence[head_op.type];

        // While parsing a function call or grouping/subexpression () we want to continue to push operators and operands within the group until we hit the closing paren.
        // Operators within the grouping symbols should follow the standard rules of comparing operator precedence and applying in their defined order. However, we want
        // to skip the comparisons of any infix or postfix operators with the grouping symbols themselves in the operator stack. Once we hit the closing paren 
        // we apply the pushed operators building the appropriate Ast using our other application function.
        
        if head_op.type == {
            case .Function_Call;               { break; }
            case .Function_Argument_Separator; { break; }
            case .Group_Start;                 { break; }
        }

        if head_op_precedence > op_precedence { 
            head_op = stack_pop(operators);
            new_operand(head_op, operands);
        }
        else { break; }
    }

    stack_push(operators,op);

    return;
}

apply_remaining_operators :: (operators: *Stack(Operator), operands: *Stack(*Ast_Node)) -> bool, *Ast_Node {

    // Apply the remaining operators left on the stack returning the AST as the last operand node on successful parse.
    while !stack_is_empty(operators) {
        op := stack_pop(operators);
        if op.type == .Group_Start || op.type == .Function_Call {
            print("Unmatched parentheses found in expression!");
            break;
        }
        
        // TODO: Robustness here!
        // "e1-" this will be parsed as the infix operator subtraction but missing the rhs operand.
        // We'd need to check the arity to make sure it requires two arguments then report that it is missing the rhs.
        new_operand(op, operands);
    }

    // On successful parse, we should have a single operand node.
    if stack_count(operands) == 1 { return true, stack_pop(operands); }
    else                          { return false, null; }
}