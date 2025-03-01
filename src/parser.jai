/*
    The parser is based off this algorithm https://github.com/erikeidt/erikeidt.github.io/blob/master/The-Double-E-Method.md.
    The first implementation was a hand-rolled recursive descent parser then I explored using a pratt parser and a combo of
    a shunting yard to convert expressions to RPN then recursive descent. The chosen method leverages two stacks so the recursion
    of normal algorithms is replaced with a simple top level loop.

    Error messages are handled in two phases. Phase 1 occurs parsing where obvious syntatic mistakes have been made.
    Examples include unmatched parenthesis or two identifiers next to each other without an infix operator, etc. Phase 2
    is a pass over the constructed AST to investigate semantic errors. These are things like trying to perform scalar operations
    between two basis elements (which is not defined), attempting to assign an expression to a scalar symbol, or making a function
    call with an unrecognized intrinsic, etc..
*/

#scope_export

/*
    When do you add a new term?
    During an addition where the factors composing the terms are not equal?
    1. How do you know you are an addition? 
        -- Parent is a binary operator node of with op addition
    2. How do you determine if the terms are equal? 
        -- You compare each factor from one term with the factors in the other term
    3. How do you know a factor is equal?
        -- If it points to the same symbol and has the same value for it's exponent
    4. How do you know if factors point to the same symbol?
        -- Their .data pointers have the same address.

// That's it then you have the symbol table with the defined symbols and what not
// L1 = L1 whatever

///////////////////////////////////////////////////////
// This is how to do sums!

    // Equal terms have equal factors.
    num_lhs_terms: int = lhs.terms.count;
    for i: 0..num_lhs_terms-1 {
        lterm := lhs.terms[i];
        for rterms in rhs.terms {
            matching_terms := check_for_matching_terms(lterm, rterm);
            if matching_terms {
                // Note: Could remove this from the rhs terms since it already matched but leaving it in for now.
                lterm.multiples += rterm.multiples;
            }
            else {
                array_add(lhs.terms, rterm);
            }
        }
    }

    matching_terms: bool = false;
    if lterm.factors.count == rterm.factors.count {
        num_matching_factors: int = 0;
        for lfactor in lterm.factors {
            for rfactor in rterm.factors {
                if lfactor.data == rfactor.data && lfactor.exponent == rfactor.exponent {
                    num_matching_factors += 1;
                }
            }
        }

        if num_matching_factors == lhs.factors.count {
            matching_terms = true;
        }
    }

    return matching_terms;

/////////////////////////////////////////////////////
// This is how to do multiplication.
Ex. 3 * 4
Ex. (4a1b2 + 3c1)e1 ^ (6a2c1 + 7d1)

// Generates whole new terms, what about in the scalar case, there are no factors
// but if we want to raise to the power then we just bump

new_terms: [..] Term;

for lterm in lhs.terms {
    for rterm in rhs.terms {
        new_term = multiply_terms(lterm, rterm);
        new_term.multiples = lterm.multiples * rterm.multiples;
        array_add(*new_terms, new_term);
    }
}

lhs.terms = new_terms;

// need to copy the factors wholesale
new_term: Term = copy_term(lterm);

found_factor: bool = false;
for rfactor in rterm.factors {
    for lfactor in new_term.factors {
        if rfactor.data == lfactor.data {
            lfactor.exponent += rfactor.exponent;
            found_factor = true;
            break;
        }
    }

    if !found_factor { array_add(new_term.factors, rfactor); }
    found_factor = false;
}

*/

// 
// Two things we have to do is:
//  1. Add scalars
//  2. Multiply scalars
//
// Scalars, want a single 'multiple' which is an integer, default to 1 i guess then handle scalars with a simple dynamic list
// it isn't the prettiest but it is dead simple.
// On add need to compare all 'scalar symbols' for a match, if they do then bump multiple
// On multiplication, multiply 'multiples' then check symbols and bump 'Power' field, (don't try to coalesce into a single power quite yet)
// Baby steps, this is already so much cleaner that i don't want to muck it up.
//
// For function call needs to be a keyword as the 'identifier' otherwise it is an error.
// Need a symbol table and it needs to hold a list of the basis one elements and we need to resolve identifiers as basis elements
//
// On top level-loop do we want to track errors and if we get an error just literally stop parsing?
// while not at the end of input || error_detected, if error detected just report the error immediately to the user
// Then we know when we go on our merry way expanding and simplifying that if something is screwed up that it was our mistake.
//
// Robustness around scalar exponentiation, can only occur after a scalar symbol!
//

/* Test cases:

v = dir(v) + pos(v)
v = dir(v) | pos(v)
v|w & w|v
(p | dir(P)!) & P   ; this is the projection formula homie!
-dir(v)!
-(dir(v)!)
dir((3+4)*(5+6))
pos((3 + 4), e1|e2) + basis(3) // NOPE
5*4^3
T = v!!
-!v! should parse as (-(!v))!
L = dir(p|v!, 4)          // this doesn't look right!
p = 4*e1; // WORKED
pos(arg1); // WORKED!
pos(arg1,arg2);
pos(arg1,arg2,arg3) ; 02.23.2025 this no workie either!
dir(arg1,(arg2+arg3));  02.23.2025 This doesn't work anymore; wonder if that was what the other logic was about? with the open paren in the infix section?
4*e1|e2 + e1|5*e3
(-a + b) * (c + d)
3 * 4 + 5 * 7
3 + 4 + 5
3^4 + 5 // doesn't work!

*/

Parser :: struct {
    tokens: [] Token;
    current_token: int;

    mode:       Parse_Mode;
    operators: *Stack(Operator);
    operands:  *Stack(*Ast_Node); 
}

parse :: (parser: *Parser, tokens: [] Token) -> Ast {

    // Only certain allocations need to persist over the lifetime of the application.
    // The AST is not one of them. If a new symbol is defined as part of an expression then the symbol table will be updated
    // on the heap otherwise on each execution of the expression loop we can destroy all previous node allocations.

    context.allocator = temp;
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
                next := peek_token(parser);
                new_operand(token, next, operands);
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
    if node.kind == .Symbol {
        symbol := cast(*Symbol_Node)node;
        print("Symbol == %\n", symbol.name);
    }
    else
    if node.kind == .Basis_Element {
        element := cast(*Basis_Element_Node)node;
       // print("Basis == %\n", element.name);
    }
    else
    if node.kind == .Scalar {
        scalar := cast(*Scalar_Node)node;
        for term: scalar.terms {
            print("Multiple == %", term.multiples);
            for factor: term.factors {
                if factor.symbol != "1" {
                    print("%", factor.symbol);
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
new_operand :: (token: Token, next: Token, operands: *Stack(*Ast_Node)) {

    node: *Ast_Node = null;
    next_op := g_nonprefix_token_to_operator[next.type];

    if token.type == .Identifier {
        node = new_symbol_node(token.text);
        // do the additional lookups in here! scalar symbol, basis element or symbol
        // and do the Symbol_Info so do a lookup and if found --> figure out if it is a basis element or scalar symbol otherwise it is just a symbol!
    }
    else
    if token.type == .Number {
        node = new_scalar_node(token.value, "1");
    }
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
            print("Unable to construct operator node, unknow arity for operator %\n", op);
            // 02.16.2025 probably should push an error node here!
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