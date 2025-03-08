#scope_export

K_Element_Type :: enum u8 {
	Origin;
	Point;
	Line;
	Plane;
	Vector;
	Bivector;
	Trivector;
	Quadvector;
}

// The K-element represents the result of an evaluated expression.
// It includes the grade of the element (multi-elements not supported atm), the type like line or bivector, and a pointer to the AST of the simplified expression.
K_Element :: struct {
	grade: int;
	type:  K_Element_Type;
	ast:  *Ast_Node;
	// TODO: 03.05.2025 could store the string representation as well.
}

Symbol_Table :: struct {
    variables: Table(string, K_Element);
}

Interpreter :: struct {
    symbol_table:   Symbol_Table;
}