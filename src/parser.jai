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

    mode:       Parse_Mode;
    operators: *Stack(Operator);
    operands:  *Stack(*Ast_Node); 
}

parse :: (parser: *Parser, tokens: [] Token) -> Ast {

    reset_parser(parser, tokens);
   
    ast: Ast;

    operators := parser.operators;
    operands  := parser.operands;

    token := peek_token(parser);
    while token.type != .Semicolon {
        token = consume_token(parser);
        
        if parser.mode == .Prefix {
            op := g_prefix_token_to_operator[token.type];
            if op {
                stack_push(operators, op);
            }
            else {
                new_operand(token, operands);
                parser.mode = .Infix_or_Postfix;
            }
        }
        else
        if parser.mode == .Infix_or_Postfix {
            op := g_nonprefix_token_to_operator[token.type];
            if !op { continue; } 
            // ^^02.23.2025  we should only continue if this is a semicolon, otherwise we could detect errors like back to back identifiers (missing operators) and other error conditions

            
            if op == .Right_Complement {
                // Post-fix operator that needs to stay infix or postfix mode since not expecting another prefix operator or operand.
                apply_operators_with_greater_precedence_then_push(op, operators, operands);
                continue;
            }
            
            if op == .Group_End {
                apply_operators_until_group_start(operators, operands);
            }
            else {
                // Apply operators with higher precedence than our current operator.
                // Once complete the operator stack is either empty or our operator has a higher precedence then the current head of the stack.
                // In either case, make our operator the new head of the operators stack.
                apply_operators_with_greater_precedence_then_push(op, operators, operands);     
                parser.mode = .Prefix;
            }
        }
    }

    // Apply the remaining operators left on the stack returning the AST as the last operand node on successful parse.
    while !stack_is_empty(operators) {
        op := stack_pop(operators);
        if op == .Group_Start || op == .Function_Call {
            print("Unmatched parentheses found in expression!");
            break;
        }
        new_operand(op, operands);
    }

    if stack_count(operands) != 1 {
        print("Operand stack did not end with a single node. Parsing failed!\n" );
    }

    ast.root = stack_pop(operands);
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

    if node.kind == .Unary_Operator {
        operator_node := cast(*Unary_Operator_Node)node;
        print( "Unary operator == %\n", operator_node.op );
        descend(operator_node.rhs, indent + 2);
    }
    else
    if node.kind == .Numeric_Literal {
        literal := cast(*Numeric_Literal_Node)node;
        print("Literal == %\n", literal.value);
    }
    else
    if node.kind == .Symbol {
        symbol := cast(*Symbol_Node)node;
        print("Symbol == %\n", symbol.name);
    }
    else
    if node.kind == .Basis_Element {
        element := cast(*Basis_Element_Node)node;
        basis   := element.basis;
        print("Basis Element (grade == %, bits == %, symbol_hash == %)\n", basis.grade, basis.bits, basis.symbol);
    }
    else
    if node.kind == .Scalar {
        scalar := cast(*Scalar_Node)node;
        print("Scalar (value == %, exponent == %, symbol_hash == %)\n", scalar.value, scalar.exponent, scalar.symbol_hash);
    }
    else
    if node.kind == .Scalar_Expression {
        scalar := cast(*Scalar_Expression_Node)node;
        for term: scalar.terms {
            print("Multiple == %", term.multiples);
            for factor: term.factors {
                if factor.symbol_hash != 1 {
                    print("%", factor.symbol_hash);
                }
            }
        }
        print("\n");
    }
    else
    if node.kind == .Binary_Operator {
        binary_operator := cast(*Binary_Operator_Node)node;
        print("Binary Operator == %\n", binary_operator.op );
        descend(binary_operator.lhs, indent + 2);
        descend(binary_operator.rhs, indent + 2);
    }
    else
    if node.kind == .Intrinsic {
        intrinsic := cast(*Intrinsic_Node)node;
        if intrinsic.type == {
            case .Direction; { print("Intrinsic: Direction\n"); }
            case .Position;  { print("Intrinsic: Position\n"); }
            case .Basis;     { print("Intrinsic: Basis\n"); }
        }
    }
}

#scope_file

Parse_Mode :: enum u8 {
    Prefix;
    Infix_or_Postfix;
}

reset_parser :: (parser: *Parser, tokens: [] Token) {

    parser.tokens = tokens;
    parser.current_token = 0;

    parser.mode      = .Prefix;
    parser.operators = stack_new(Operator);
    parser.operands  = stack_new(*Ast_Node); 
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

check_for_operand :: (expected: Node_Kind, operands: *Stack(*Ast_Node)) -> bool {
    return stack_peek(operands).kind == expected;
}

// There are two ways to generate a new operand on the operands stack.
//  (1) Parse a token that is the left or right-hand side of an operator such as an identifier, basis element, scalar, literal, etc.
//  (2) Applying an operator to operands within the stack generating a new operand node.
new_operand :: (token: Token, operands: *Stack(*Ast_Node)) {

    node: *Ast_Node = null;

    if token.type == .Scalar {
        node = new_scalar_node(token.scalar);
    }
    else 
    if token.type == .Basis_Element {
        scalar: Scalar = .{ value = 1, exponent = 1, symbol_hash = hash_scalar_symbol("1") };
        scalar_node   :=  new_scalar_node(scalar);

        node = new_basis_element_node(scalar_node, token.basis);
    }
    else
    if token.type == .Identifier {
        node = new_symbol_node(token.text);
    }
    else
    if token.type == .Keyword_Direction {
        node = new_intrinsic_node(.Direction);
    }
    else
    if token.type == .Keyword_Position {
        node = new_intrinsic_node(.Position);
    }
    else
    if token.type == .Keyword_Basis {
        node = new_intrinsic_node(.Basis);
    }

    stack_push(operands, node);
    return;
}

new_operand :: (op: Operator, operands: *Stack(*Ast_Node)) {

    // Build the operator AST node using the required number of operands.
    // Push the constructed node onto the operand stack.

    node: *Ast_Node = null;
    if g_operator_arity[op] == {
        case 1; {
            rhs := stack_pop(operands);
            node = new_unary_operator_node(op, rhs);
        }
        case 2; {
            rhs := stack_pop(operands);
            lhs := stack_pop(operands);
            node = new_binary_operator_node(lhs, op, rhs);
        }
        case; {
            // 02.16.2025 probably should push an error node here!
            print("Unable to construct operator node, unknow arity for operator %\n", op);
        }
    }

    stack_push(operands, node);
    
    return;          
}

apply_operators_until_group_start :: (operators: *Stack(Operator), operands: *Stack(*Ast_Node)) {

    // Build new operand nodes with the operator at the head of the stack until we reach either .Group_Start or .Function_Call.
    // If we reach the end of the stack before finding a match then the expression has an unmatched open parenthesis.

    while !stack_is_empty(operators) {
        op := stack_pop(operators);
        if op == {
            case .Group_Start; { return; }
            case .Function_Call; {
                // Function call can either be of the form 'functionName(arg1)' or 'functionName(arg1,arg2,arg3,..)
                // In each case the Function Call is a binary operator whose lhs is the function name and the rhs depends on the cases above.
                //  (1) Single arg will have the rhs of the binary operator node for the function call be 'arg1'
                //  (2) Multi  arg will have the rhs of the binary operator node for the function call be another binary operator of type 'Function_Argument_Separator'
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

    op_precedence := g_operator_precedence[op];
    while !stack_is_empty(operators) {
        head_op            := stack_peek(operators);
        head_op_precedence := g_operator_precedence[head_op];

        // While parsing a function call or grouping/subexpression () we want to continue to push operators and operands within the group until we hit the closing paren.
        // Operators within the grouping symbols should follow the standard rules of comparing operator precedence and applying in their defined order. However, we want
        // to skip the comparisons of any infix or postfix operators with the grouping symbols themselves in the operator stack. Once we hit the closing paren 
        // we apply the pushed operators building the appropriate Ast using our other application function.
        
        if head_op == {
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