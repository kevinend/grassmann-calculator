#scope_export

Operator :: enum u8 {
    No_Op;
    Assignment;
    Addition;
    Negation;
    Subtraction;
    Scalar_Multiplication;
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

// These are sparse static tables intialized a single time on application startup and used throughout the lifecycle of the application by the parser.
token_to_prefix_operator           : [Token_Type.Count] Operator;
token_to_infix_or_postfix_operator : [Token_Type.Count] Operator;
operator_precedence                : [Operator.Count]   u8;
operator_arity                     : [Operator.Count]   u8;

initialize_operator_tables :: () {
    
    // Map tokens in the prefix position to their corresponding unary operator.
    // Table will be default initialied to zero (No_Op) so all non-operator tokens will return No_Op.
    {
        token_to_prefix_operator[Token_Type.Minus]               = .Negation;
        token_to_prefix_operator[Token_Type.Exclamation_Point]   = .Left_Complement;
        token_to_prefix_operator[Token_Type.Open_Paren]          = .Group_Start;
    }

    // Map tokens in the infix or postfix position to their corresponding operators.
    // Table will be default initialied to zero (No_Op) so all non-operator tokens will return No_Op.
    {
        token_to_infix_or_postfix_operator[Token_Type.Equals]             = .Assignment;
        token_to_infix_or_postfix_operator[Token_Type.Plus]               = .Addition;
        token_to_infix_or_postfix_operator[Token_Type.Minus]              = .Subtraction;
        token_to_infix_or_postfix_operator[Token_Type.Asterisk]           = .Scalar_Multiplication;
        token_to_infix_or_postfix_operator[Token_Type.Forward_Slash]      = .Scalar_Division;
        token_to_infix_or_postfix_operator[Token_Type.Caret]              = .Scalar_Exponentiation;

        token_to_infix_or_postfix_operator[Token_Type.Pipe]               = .Exterior_Product;
        token_to_infix_or_postfix_operator[Token_Type.Ampersand]          = .Regressive_Product;
        token_to_infix_or_postfix_operator[Token_Type.Exclamation_Point]  = .Right_Complement;
        token_to_infix_or_postfix_operator[Token_Type.Open_Paren]         = .Function_Call;
        token_to_infix_or_postfix_operator[Token_Type.Closed_Paren]       = .Group_End;
        token_to_infix_or_postfix_operator[Token_Type.Comma]              = .Function_Argument_Separator;
        
    }

    // Map an operator to its precedence relative to other operators.
    {
        // This table determines how operators are grouped. When comparing two operators, the operator with the higher precedence (larger integer value)
        // is applied before the operator with the lower precedence. As example, 3 * 4 + 5 will be parsed as (3*4) + 5 since * has a higher
        // precedence than +. If the operators have the same precedence then it is left-associative so -!e1 would be parsed as -(!e1) since left-complement
        // and negation have the same precedence.
        operator_precedence[Operator.Assignment]                  = 1;
        operator_precedence[Operator.Function_Argument_Separator] = 2;
        operator_precedence[Operator.Addition]                    = 3;
        operator_precedence[Operator.Subtraction]                 = 3;
        operator_precedence[Operator.Regressive_Product]          = 4;
        operator_precedence[Operator.Exterior_Product]            = 5;
        operator_precedence[Operator.Scalar_Multiplication]       = 6;
        operator_precedence[Operator.Scalar_Division]             = 7;
        operator_precedence[Operator.Scalar_Exponentiation]       = 8;
        operator_precedence[Operator.Right_Complement]            = 9;
        operator_precedence[Operator.Left_Complement]             = 10;
        operator_precedence[Operator.Negation]                    = 10;
        operator_precedence[Operator.Function_Call]               = 11;
        
        operator_precedence[Operator.Group_Start]                 = 11;
        operator_precedence[Operator.Group_End]                   = 11;
    }

    // Map an operator to the number of arguments it takes which is known as a function's arity.
    {
        operator_arity[Operator.Assignment]                   = 2;
        operator_arity[Operator.Addition]                     = 2;
        operator_arity[Operator.Negation]                     = 1;
        operator_arity[Operator.Subtraction]                  = 2;
        operator_arity[Operator.Scalar_Multiplication]        = 2;
        operator_arity[Operator.Scalar_Division]              = 2;
        operator_arity[Operator.Scalar_Exponentiation]        = 2;
        operator_arity[Operator.Exterior_Product]             = 2;
        operator_arity[Operator.Regressive_Product]           = 2;
        operator_arity[Operator.Left_Complement]              = 1;
        operator_arity[Operator.Right_Complement]             = 1;
        operator_arity[Operator.Group_Start]                  = 0;
        operator_arity[Operator.Function_Call]                = 2;
        operator_arity[Operator.Group_End]                    = 0;
        operator_arity[Operator.Function_Argument_Separator]  = 2;
    }

    return;
}
