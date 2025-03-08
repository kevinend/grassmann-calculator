/*
    This file contains information about the currently declared exterior algebra space.
    The space defines an 'execution-context' for the current set of expressions.

    The exterior (Grassmann) algebra components tied to the space are the dimension of the current execution context,
    the list of basis-elements of grades 0 to n and a mapping table from the integer representation of the basis elements
    to their 'canonical' ordering. Ordering is explained further down in this comment.

    The basis 'objects' are referred to as elements rather than vectors. In Grassmann algebra, the basis set for a 
    space can include the origin (a point) along with the typical 1-vectors, so the term basis "element" is used rather
    than basis vectors.

	All Basis elements are built using the 1-elements which are currently encoded as bit fields.
	Take G(R4) as an example:
		e1 = 1 = (1 << 1)
		e2 = 2 = (1 << 2)
		e3 = 4 = (1 << 3)
		e4 = 8 = (1 << 4)

	In Grassmann Algebra, a sum of elements of the same grade is known as a k-vector or k-element depending on the space.
    There is an extension of this idea with sums of elements of different grades known as a multi-vector or multi-element.

    In this application, a k-element or multi-element is ordered from the lowest to highest graded basis elements.
    The order of the basis 1-elements supplied by the user or defaulted on program start impose the order of the basis
    elements of the same grade.

	Example in G(R4) with 1-elements [e1,e2,e3,e4] and non-zero scalar values for each basis element:
		Ex. MultiElement = e1 + e2 + e3 + e4 + e1^e2 + e1^e3 + e1^e4 + e2^e3 + e2^e4 + e3^e4 + e1^e2^e3 + e1^e2^e4 + e1^e3^e4 + e2^e3^e4 + e1^e2^e3^e4

	Example with missing terms. Same ordering is imposed.
		Ex. MultiElement = e1 + e4 + e1^e3 + e1^e2^e3^e4

	The space contains a mapping table from the bit representation to the ordering above since the bits do not naturally enforce the order
    Ex. e1 = 1, e3 = 4 and e1^e2 = 3 which would naturally impose the order e1, e1^e2, e3 which does not coincide with the above.
*/

#scope_export

Basis_Element :: struct {
    grade:  int;
    bits:   int;
	symbol: string;
}

Scalar :: struct {

/*
    A scalar can either be a value like 3 or a symbol like 'a1'. 
    This type has a 'symbol_hash' field which is defined 
    to be either "1" (for values) or a single leading character followed by up to 3 digits (ex. a1, a01, a001, A1, A01, A001 etc.).

    Instead of the symbol field being a string or a pointer to an interned string the symbol is encoded as a u16.
    That u16 is stored in the 'Scalar_Symbol's data structure which uses the u16 as a key and returns the string representation of the symbol
    when we need to print the scalar to the end-user. All calculations use the u16 representation.

    For some reason using a 16-byte string, or an 8-byte pointer to a string seemed too wasteful when the restricted symbols 
    can be represented as 2-byte shorts.
*/

    value:       float;
    exponent:    s16;
    symbol_hash: u16;
}

Scalar_Symbols :: struct {
    hashes:  [..] u16;     
    symbols: [..] string;  // ex. "1", "a1", "b2", etc.
}

Exterior_Space :: struct {

	MAX_DIMENSION :: 6;
	MAX_NUMBER_OF_BASIS_ELEMENTS :: #run power(2, MAX_DIMENSION);

	dimension: int;
	n_element: *Basis_Element;

	basis_elements:         [MAX_NUMBER_OF_BASIS_ELEMENTS]  Basis_Element;
	bits_to_basis_elements: [MAX_NUMBER_OF_BASIS_ELEMENTS] *Basis_Element;

    scalar_symbols: Scalar_Symbols;
}

space: Exterior_Space; // TODO: could pass this to the lexer... and the interpreter that's when we need both!

update_exterior_space :: (dimension: int, _1_element_symbols: [] string) {

	assert(dimension <= Exterior_Space.MAX_DIMENSION);
    assert(dimension == _1_element_symbols.count);

	space.dimension = dimension;

	num_basis_elements := power(2, dimension);
	
	space.basis_elements[0] = Basis_Element.{ grade = 0, bits = 0, symbol = "1" };
	count: int = 1;
	for j: 1..dimension {
		generate_basis_elements(
			 i 		 = 0
			,element = Basis_Element.{ grade = 0, bits = 0 }
			,n       = dimension
			,k	     = j
			,c       = *count
		);
	}

	assert(count == num_basis_elements);

	for i: 1..num_basis_elements-1 {
		element        := *space.basis_elements[i];
		element.symbol =   generate_basis_symbol(element.bits, element.grade, _1_element_symbols);
	}
	
	// update the mapping table from an element's bits to the basis element
	for i: 0..num_basis_elements-1 {
		index := space.basis_elements[i].bits;
		space.bits_to_basis_elements[index] = *space.basis_elements[i];
	}

	space.n_element = *space.basis_elements[num_basis_elements-1];

	return;
}

hash_scalar_symbol :: (s: string) -> u16 {

    // Format of a scalar symbol is an alphabet character followed by up to 3 digits
    // The smallest number is a000 and the largest is Z999.
    // We map a-z to the numbers 1-26 and A-Z as 26-53.
    // As long as we keep this range we can fit all the scalar symbols hashes into u16s.
    //
    // The hash value 1 is reserved for the symbol "1" which is used to represent pure numbers.
    //
    // To build up the number we start at end the end of the string and parse digits as powers of 10
    // then convert the character and add its hash.

    assert(s[0] == #char "1" || (s[0] >= #char "a" && s[0] <= #char "z") || (s[0] >= #char "A" && s[0] <= #char "Z"));
    assert(s.count <= 4);

    if s[0] == #char "1" {
        return 1;
    }

    i := s.count-1;

    hash:  u16;
    powers: u16 = 1;
    digit:  u16;
    while i > 0 {
        digit  = s[i] - #char "0";
        digit  *= powers;
        hash  += digit;

        powers *= 10;
        
        i-= 1;
    }

    if s[0] >= #char "a" && s[0] <= #char "z" {
        digit  = s[i] - #char "a" + 1;
        digit *= powers;
        hash += digit;
    }
    else {
        digit  = (s[i] - #char "A") + 27;
        digit *= powers;
        hash += digit; 
    }

    assert( hash <= 65535 );
    return hash;
}

insert_scalar_symbol :: (scalar_symbols: *Scalar_Symbols, hash: u16, symbol: string) {

    // Linear runtime.
    // The size of the hashes are 2-bytes so you could fit 256 symbols on 4, 64 byte cache lines so not worried about it for now.
    found: bool = false;
    for i: 0..scalar_symbols.hashes.count-1 {
        if hash == scalar_symbols.hashes[i] {
            found = true;
            break;
        }
    }
    
    if !found {
        array_add(*scalar_symbols.hashes,  hash);
        array_add(*scalar_symbols.symbols, symbol);
    }

    return;
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
	 i: int
	,element: Basis_Element
	,n: int
	,k: int
	,c: *int
) {
	
	// This is a modified version of n choose k, where for each element in our 1-elements we generate the combinations
	// with the element and the combinations without the element. The caller invokes this for k = 1..N so we end up
	// with each basis element in our collection in order of grade. Also, the 1-elements themselves are NOT supplied
	// as part of the call. Each 1-element is a bit field (1 << i) so when we bump i by one we are effectively dealing with
	// the next 1-element.

	if element.grade == k {
		// found a basis element of grade k, add to our collection of basis elements
		space.basis_elements[c.*] = element;
		c.* += 1;
		return;
	}

	if i >= n {
		// reached the end of the 1-elements without generating an element of grade k
		return;
	}

	// include the current one element (represented as 2^i so just a bit-shift) and find the basis-elements with this 1-element and do the same excluding this current 1-element
	include_first := Basis_Element.{ grade = element.grade + 1, bits = element.bits | (1 << i) }; 
	exclude_first := Basis_Element.{ grade = element.grade    , bits = element.bits };

	generate_basis_elements(i+1, include_first, n, k, c);
	generate_basis_elements(i+1, exclude_first, n, k, c);
}

generate_basis_symbol :: (bits: int, grade: int, _1_element_symbols: [] string) -> string {

	builder: String_Builder;

	wedge := "^";
	i 	  := 0;
	n 	  := 0;
	
	while true {
		if bits & (1 << i) {
			if n == grade-1 { break; }
			else {
				append(*builder, _1_element_symbols[i]);
				append(*builder, wedge);
				n += 1;
			}
		}
					
		i += 1;
	}
	append(*builder, _1_element_symbols[i]);

	return builder_to_string(*builder);
}