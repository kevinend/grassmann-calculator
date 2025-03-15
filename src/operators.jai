#scope_export

Operator_Type :: enum u8 {
    No_Op;
    Assignment;
    Addition;
    Negation;
    Subtraction;
    Scalar_Multiplication; // TODO: Should this be removed and replaced with a PIPE?
    Scalar_Division;
    Scalar_Exponentiation;
    Exterior_Product;
    Regressive_Product;
    Left_Complement;
    Right_Complement;
    Group_Start;
    Function_Call;
    Group_End;
    Function_Argument_Separator;
    
    Count;
}

Operator :: struct {
    type: Operator_Type;
    position: Position_Info;
}

initialize_operator_tables :: () {
    
    // Map tokens in the prefix position to their corresponding unary operator.
    // Table will be default initialied to zero (No_Op) so all non-operator tokens will return No_Op.
    {
        token_to_prefix_operator[Token_Type.Minus]               = .{ type = .Negation };
        token_to_prefix_operator[Token_Type.Exclamation_Point]   = .{ type = .Left_Complement };
        token_to_prefix_operator[Token_Type.Open_Paren]          = .{ type = .Group_Start };
    }

    // Map tokens in the infix or postfix position to their corresponding operators.
    // Table will be default initialied to zero (No_Op) so all non-operator tokens will return No_Op.
    {
        token_to_infix_or_postfix_operator[Token_Type.Equals]             = .{ type = .Assignment };
        token_to_infix_or_postfix_operator[Token_Type.Plus]               = .{ type = .Addition   };
        token_to_infix_or_postfix_operator[Token_Type.Minus]              = .{ type = .Subtraction };
        token_to_infix_or_postfix_operator[Token_Type.Asterisk]           = .{ type = .Scalar_Multiplication };
        token_to_infix_or_postfix_operator[Token_Type.Forward_Slash]      = .{ type = .Scalar_Division };
        token_to_infix_or_postfix_operator[Token_Type.Caret]              = .{ type = .Scalar_Exponentiation };

        token_to_infix_or_postfix_operator[Token_Type.Pipe]               = .{ type = .Exterior_Product };
        token_to_infix_or_postfix_operator[Token_Type.Ampersand]          = .{ type = .Regressive_Product };
        token_to_infix_or_postfix_operator[Token_Type.Exclamation_Point]  = .{ type = .Right_Complement };
        token_to_infix_or_postfix_operator[Token_Type.Open_Paren]         = .{ type = .Function_Call };
        token_to_infix_or_postfix_operator[Token_Type.Closed_Paren]       = .{ type = .Group_End };
        token_to_infix_or_postfix_operator[Token_Type.Comma]              = .{ type = .Function_Argument_Separator };
        
    }

    // Map an operator to its precedence relative to other operators.
    {
        // This table determines how operators are grouped. When comparing two operators, the operator with the higher precedence (larger integer value)
        // is applied before the operator with the lower precedence. As example, 3 * 4 + 5 will be parsed as (3*4) + 5 since * has a higher
        // precedence than +. If the operators have the same precedence then it is left-associative so -!e1 would be parsed as -(!e1) since left-complement
        // and negation have the same precedence.
        operator_precedence[Operator_Type.Assignment]                  = 1;
        operator_precedence[Operator_Type.Function_Argument_Separator] = 2;
        operator_precedence[Operator_Type.Addition]                    = 3;
        operator_precedence[Operator_Type.Subtraction]                 = 3;
        operator_precedence[Operator_Type.Regressive_Product]          = 4;
        operator_precedence[Operator_Type.Exterior_Product]            = 5;
        operator_precedence[Operator_Type.Scalar_Multiplication]       = 6;
        operator_precedence[Operator_Type.Scalar_Division]             = 7;
        operator_precedence[Operator_Type.Scalar_Exponentiation]       = 8;
        operator_precedence[Operator_Type.Right_Complement]            = 9;
        operator_precedence[Operator_Type.Left_Complement]             = 10;
        operator_precedence[Operator_Type.Negation]                    = 10;
        operator_precedence[Operator_Type.Function_Call]               = 11;
        operator_precedence[Operator_Type.Group_Start]                 = 11;
        operator_precedence[Operator_Type.Group_End]                   = 11;
    }

    // Map an operator to the number of arguments it takes which is known as a function's arity.
    {
        operator_arity[Operator_Type.Assignment]                   = 2;
        operator_arity[Operator_Type.Addition]                     = 2;
        operator_arity[Operator_Type.Negation]                     = 1;
        operator_arity[Operator_Type.Subtraction]                  = 2;
        operator_arity[Operator_Type.Scalar_Multiplication]        = 2;
        operator_arity[Operator_Type.Scalar_Division]              = 2;
        operator_arity[Operator_Type.Scalar_Exponentiation]        = 2;
        operator_arity[Operator_Type.Exterior_Product]             = 2;
        operator_arity[Operator_Type.Regressive_Product]           = 2;
        operator_arity[Operator_Type.Left_Complement]              = 1;
        operator_arity[Operator_Type.Right_Complement]             = 1;
        operator_arity[Operator_Type.Group_Start]                  = 0;
        operator_arity[Operator_Type.Function_Call]                = 2;
        operator_arity[Operator_Type.Group_End]                    = 0;
        operator_arity[Operator_Type.Function_Argument_Separator]  = 2;
    }

    return;
}

// Attempts to convert a token into an operator. If there is no mapping from the token to an operator
// then it will return 0 or 'No_Op' from the lookup.
attempt_to_convert_token_to_prefix_operator :: (token: Token) -> Operator {
    
    op: Operator;
    op.position = token.position;
    return token_to_prefix_operator[token.type];
}

attempt_to_convert_token_to_infix_or_postfix_operator :: (token: Token) -> Operator {

    op: Operator;
    op.position = token.position;
    return token_to_infix_or_postfix_operator[token.type];
}

is_operator :: (op: Operator) -> bool {
    return op.type != .No_Op;
}

get_operator_precedence :: (op: Operator) -> u8 {
    return operator_precedence[op.type];
}
get_operator_arity :: (op: Operator) -> u8 {
    return operator_arity[op.type];
}

#scope_file

// These are sparse static tables intialized a single time on application startup and used throughout the lifecycle of the application by the parser.
token_to_prefix_operator           : [Token_Type.Count]      Operator;
token_to_infix_or_postfix_operator : [Token_Type.Count]      Operator;
operator_precedence                : [Operator_Type.Count]   u8;
operator_arity                     : [Operator_Type.Count]   u8;