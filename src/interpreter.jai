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

Basis_Element :: struct {
    grade: int;
    bits:  int;
	symbol: string;
}

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

Exterior_Space :: struct {
    // Grassmann/Exterior-space specific configurations.
	MAX_DIMENSION :: 6;
	MAX_NUMBER_OF_BASIS_ELEMENTS :: #run power( 2, MAX_DIMENSION );

	dimension: int;
	n_element: *Basis_Element;

	basis_elements:         [MAX_NUMBER_OF_BASIS_ELEMENTS]  Basis_Element;
	bits_to_basis_elements: [MAX_NUMBER_OF_BASIS_ELEMENTS] *Basis_Element;
}

Symbol_Table_Partition :: enum u8 {
    Scalars;
    Basis_Elements;
    Symbols;
}

Symbol_Table :: struct {
    _1_elements:  [] Basis_Element;			// currently declared set of basis 1 elements such as e0,e1,e2,e3
    _0_elements:  Table(string, string);    // interned strings for scalar symbols like 'a1','b2', etc.; can compare pointers instead of strings during scalar operations.
    symbols:      Table(string, K_Element); // user-defined symbols generated from expressions like v = e1^e2; where v is the symbol stored in this table.
}

Interpreter :: struct {
    exterior_space: Exterior_Space;
    symbol_table:   Symbol_Table;
}

// Queries to the symbol table can return different types so do a little subclassing just like the AST nodes.
Symbol_Kind :: enum u8 {
	Basis_Element;
	Scalar;
	K_Element;
}

Symbol :: struct {
	kind: Symbol_Kind;
}

Basis_Element_Symbol :: struct {
	#as using _kind: Symbol;
	basis_element: *Basis_Element;
}

Scalar_Symbol :: struct {
	#as using _kind: Symbol;
	data: string;
}

User_Defined_Symbol :: struct {
	#as using _kind: Symbol;
	root: *Ast_Node;
}

update_exterior_space :: (exterior_space: *Exterior_Space, dimension: int, _1_element_symbols: [] string) {

	assert(dimension <= Exterior_Space.MAX_DIMENSION);

	exterior_space.dimension = dimension;

	num_basis_elements := power(2, dimension);
	
	exterior_space.basis_elements[0] = Basis_Element.{ grade = 0, bits = 0, symbol = "1" };
	count: int = 1;
	for j: 1..dimension {
		generate_basis_elements(
             exterior_space  // 02.26 -- Can i pass in the list of basis_elements so we update directly?
			,i 		 = 0
			,element = Basis_Element.{ grade = 0, bits = 0 }
			,n       = dimension
			,k	     = j
			,c       = *count
		);
	}

	assert(count == num_basis_elements);

	for i: 1..num_basis_elements-1 {
		element        := exterior_space.basis_elements[i];
		element.symbol =  generate_basis_symbol(element.bits, element.grade, _1_element_symbols);
	}
	
	// update the mapping table from an element's bits to the basis element
	for i: 0..num_basis_elements-1 {
		index := exterior_space.basis_elements[i].bits;
		exterior_space.bits_to_basis_elements[index] = *exterior_space.basis_elements[i];
	}

	exterior_space.n_element = *exterior_space.basis_elements[num_basis_elements-1];

	return;
}

lookup_symbol :: (symbol: string) -> bool, *Symbol {

	found: bool = false;
	symbol: *Symbol = null;

	return found, symbol;
/*
	// Lookup a symbol and check if it is a basis 1-element of the space. If so return it, else return null.
	found:   bool = false;
	element: *Basis_Element = null;

	for i: 1..exterior_space.dimension {
		if symbol == exterior_space.basis_elements[i].symbol {
			found = true;
			element = *exterior_space.basis_elements[i];
			break;
		}
	}

	return found, element;
*/
}


#scope_file

power :: (a: int, b: int) -> int {
	
	// Implements the basic arithment expression: c = a^b
	c: int = 1;
	for i: 0..b-1 {
		c *= a;
	}
	return c;
}

choose :: (n: int, k: int) -> (value: int) {
	// example: choose(4,2) == (4*3)/(2*1) == 6
    if k == 0 return 1;
    return (n * choose(n-1, k-1)) / k;
}

generate_basis_elements :: (
     exterior_space: *Exterior_Space
	,i: int
	,element: Basis_Element
	,n: int
	,k: int
	,c: *int
) {
	
	// This is a modified version of n choose k, where for each element in our one-elements we generate the combinations
	// with the element and the combinations without the element. The caller invokes this for k = 1..N so we end up
	// with each basis element in our collection in order of grade. Also, the one elements themselves are NOT supplied
	// as part of the call. Each one element is a bit field (1 << i) so when we bump i by one we are effectively dealing with
	// the next one element.

	if element.grade == k {
		// found a basis element of grade k, add to our collection of basis elements
		exterior_space.basis_elements[c.*] = element;
		c.* += 1;
		return;
	}

	if i >= n {
		// reached the end of the one elements without generating an element of grade k
		return;
	}

	// include the current one element (represented as 2^i so just a bit-shift) and find the basis-elements with this one-element and do the same excluding this current one-element
	include_first := Basis_Element.{ grade = element.grade + 1, bits = element.bits | (1 << i) }; 
	exclude_first := Basis_Element.{ grade = element.grade    , bits = element.bits };

	generate_basis_elements(exterior_space, i+1, include_first, n, k, c);
	generate_basis_elements(exterior_space, i+1, exclude_first, n, k, c);
}

generate_basis_symbol :: (bits: int, grade: int, _1_element_symbols: [] string) -> string {

	builder: String_Builder;
	
	wedge := "^";
	i := 0;
	n := 0;
	while n < grade-1 {
		if bits & (1 << i) {
			append(*builder, _1_element_symbols[i]);
			append(*builder, wedge);
			n += 1;
		}
		
		i += 1;
	}
	append(*builder, _1_element_symbols[i]);

	return builder_to_string(*builder);
}
