#scope_export

Token_Type :: enum u32 {

    Undefined;

    Equals;             // prefix = No_Op,           infix_or_postfix = Assignment
    Plus;               // prefix = No_Op,           infix_or_postfix = Addition
    Minus;              // prefix = Negation,        infix_or_postfix = Subtraction
    Asterisk;           // prefix = No_Op,           infix_or_postfix = Scalar_Multiplication
    Forward_Slash;      // prefix = No_Op,           infix_or_postfix = Scalar_Division (could support grassmann division in the future)
    Caret;              // prefix = No_Op,           infix_or_postfix = Scalar_Exponentiation
    Pipe;               // prefix = No_Op,           infix_or_postfix = Exterior_Product
    Ampersand;          // prefix = No_Op,           infix_or_postfix = Regressive_Product
    At_Symbol;          // prefix = No_Op,           infix_or_postfix = Interior_Product
    Exclamation_Point;  // prefix = Left_Complement, infix_or_postfix = Right_Complement
    Open_Paren;         // prefix = Group_Start,     infix_or_postfix = Function_Call
    Closed_Paren;       // prefix = No_Op,           infix_or_postfix = Function_Call or End of Group
    Comma;              // prefix = No_Op,           infix_or_postfix = Argument_Separator
    Semicolon;          // prefix = No_Op,           infix_or_postfix = No_Op

    Literal;
    Number;
    Identifier;

    Keyword_Direction;
    Keyword_Position;
    Keyword_Basis;

    Error;

    Count;
}

Position_Info :: struct {

    line_number: int;
    char_start:  int;
    char_end:    int;
}

Token :: struct {
    
    type: Token_Type;
    position: Position_Info;
    union {
        text:    string;
        value:   float;
    }
}