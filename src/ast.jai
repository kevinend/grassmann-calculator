#scope_export

// 02.23.2025 KCE Operators are rolled up into either a unary or binary operator node.
// This isn't a problem but requires more recursion when evaluating nodes because you have to figure
// out if it is an operator at all then dispatch based on the type of operator. The primary benefit is
// it keeps the number of Ast_Node types low. We could do 1-to-1 operator to node type but that requires
// additional changes like a table to map an operator to it's 'new-node' function based on the current
// implementation. Doesn't seem worth it at this time.

Node_Kind :: enum u8 {
    Symbol;
    Basis_Element;
    Scalar;
    Unary_Operator;   // prefix operators
    Binary_Operator;  // infix/postfix operators
    Intrinsic;
}

Ast_Node :: struct {
    kind: Node_Kind;
}

Ast :: struct {
    root: *Ast_Node;
}

Symbol_Node :: struct {
    #as using _kind: Ast_Node;
    name: string;
}

Basis_Element_Node :: struct {
    #as using _kind: Ast_Node;
	
	coefficient: *Scalar_Node;	
	basis:       *Basis_Element;
}

Factor :: struct {
    exponent: int;
    symbol: string;
}

Term :: struct {
    multiples: int;
    factors: [..] Factor;
}

Scalar_Node :: struct {
    #as using _kind: Ast_Node;
    terms: [..] Term;
}

Unary_Operator_Node :: struct {
    #as using _kind: Ast_Node;
    op:  Operator;
    rhs: *Ast_Node;
}

Binary_Operator_Node :: struct {
    #as using _kind: Ast_Node;
    lhs: *Ast_Node;
    op:  Operator;
    rhs: *Ast_Node;
}

// Keywords get mapped to intrinsic node types.
// Intrsics are predefined functions/commands that user's can call within an expression.
Intrinsic_Type :: enum u8 {
    Direction;
    Position;
    Basis;
}

Intrinsic_Node :: struct {
    #as using _kind: Ast_Node;
    type: Intrinsic_Type;
}

new_symbol_node :: (name: string) -> *Ast_Node {

    node: *Symbol_Node = alloc(size_of(Symbol_Node));
    assert(node != null);

    node.kind = Node_Kind.Symbol;
    node.name = name;
    return node;
}

new_basis_element_node :: (coefficient: *Scalar_Node, basis_element: *Basis_Element) -> *Ast_Node {

    node: *Basis_Element_Node = alloc(size_of(Basis_Element_Node));
    assert(node != null);

    node.kind        = Node_Kind.Basis_Element;
    node.coefficient = coefficient;
    node.basis       = basis_element;

    return node;
}

new_scalar_node :: (value: int, symbol: string) -> *Ast_Node {

    node: *Scalar_Node = alloc(size_of(Scalar_Node));
    assert(node != null);

    factor: Factor;
    term: Term;
    
    if symbol == "1" {
        factor.exponent = 0;
        factor.symbol   = symbol;

        term.multiples = value;
    }
    else {
        factor.exponent = 1;
        factor.symbol   = symbol;

        term.multiples  = 1;
    }

    factors: [..] Factor;
    terms:   [..] Term;

    array_add(*factors, factor);
    term.factors = factors;

    array_add(*terms, term);

    node.kind  = Node_Kind.Scalar;
    node.terms = terms;

    return node;
}

new_unary_operator_node :: (op: Operator, rhs: *Ast_Node) -> *Ast_Node {

    node: *Unary_Operator_Node = alloc(size_of(Unary_Operator_Node));
    if !node { print( "Error!\n" ); }

    node.kind = Node_Kind.Unary_Operator;
    node.op = op;
    node.rhs = rhs;

    return node;
}

new_binary_operator_node :: (lhs: *Ast_Node, op: Operator, rhs: *Ast_Node) -> *Ast_Node {

    node: *Binary_Operator_Node = alloc(size_of(Binary_Operator_Node));
    if !node { print("Error!\n" );}

    node.kind = Node_Kind.Binary_Operator;
    node.lhs = lhs;
    node.op = op;
    node.rhs = rhs;

    return node;
}

new_intrinsic_node :: (type: Intrinsic_Type) -> *Ast_Node {

    node: *Intrinsic_Node = alloc(size_of(Intrinsic_Node));
    if !node { print("Error!\n"); }

    node.kind = Node_Kind.Intrinsic;
    node.type = type;

    return node;
}