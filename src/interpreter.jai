/*
    The interpreter serves as the 'execution-environment' for the application.
    It combines sectioned symbol table(s) and data pertaining to the currently declared exterior algebra space.

    The symbol table is partitioned based on symbol types because this application can have identifiers that represent
    basis-elements (such as e1, e2, etc), symbolic scalars (a1, b2, etc), and user-defined symbols.

    The exterior (Grassmann) algebra components in the interpreter are the dimension of the current execution context,
    the list of basis-elements of grades 0 to n and a mapping table from the integer representation of the basis elements
    to their 'canonical' ordering. Ordering is explained further down in this comment.

    The basis 'objects' are referred to as elements rather than vectors. In Grassmann algebra, the basis set for a 
    space can include the origin (a point) along with the typical 1-vectors, so the term basis "element" is used rather
    than basis vectors.

	All Basis elements are built using the 1-elements which are currently encoded as BIT FIELDS.
	Take G(R4) as an example:
		e1 = 1 = (1 << 1)
		e2 = 2 = (1 << 2)
		e3 = 4 = (1 << 3)
		e4 = 8 = (1 << 4)

	In Grassmann Algebra, a sum of elements of the same grade is known as a k-vector or k-element depending on the space.

	The sum of k-vectors of differing grades is known as a multivector, by extension we can have
	multielements. A multielement is a sum of k-elements of differing grades. For example e1 + e1^e2.

	In this application, a 'multielement' order is defined. That is, the basis elements composing a multielement
	are ordered such that the final result of an expression is a multielement with this order imposed.
	
	The order is from the lowest graded elements to the highest graded elements for the declared dimension with
	the additional restriction that the order of the basis one elements supplied by the user or defaulted on program
	start impose the order of the basis elements of the same grade.

	As an example in G(R4) with 1-elements [e1,e2,e3,e4], a multielement with non-zero scalar values for each basis element would look like the following.
		Ex. MultiElement = e1 + e2 + e3 + e4 + e1^e2 + e1^e3 + e1^e4 + e2^e3 + e2^e4 + e3^e4 + e1^e2^e3 + e1^e2^e4 + e1^e3^e4 + e2^e3^e4 + e1^e2^e3^e4

	If terms are missing from the multielement (zero scalar values) then the final expression will be a subset of the above but still in the same order.
		Ex. MultiElement = e1 + e4 + e1^e3 + e1^e2^e3^e4

	The interpreter contains a mapping table from the bit representation to the ordering above since the bits do not naturally enforce the order
    Ex. e1 = 1, e3 = 4 and e1^e2 = 3 which would naturally impose the order e1, e1^e2, e3 which does not coincide with the above.
*/

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


Symbol_Table_Partition :: enum u8 {
    Scalars;
    Basis_Elements;
    Symbols;
}

Symbol_Table :: struct {
    variables: Table(string, K_Element); // user-defined symbols generated from expressions like v = e1^e2; where v is the symbol stored in this table.
}

Interpreter :: struct {
    symbol_table:   Symbol_Table;
}

User_Defined_Symbol :: struct {
	root: *Ast_Node;
}

lookup_symbol :: (symbol: string) -> bool {

	found: bool = false;
	return found;
}