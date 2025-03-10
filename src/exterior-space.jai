/*
    This file contains information about the currently declared exterior algebra space.
    The space defines an 'execution-context' for the current set of expressions.
	Part of the execution context is the list of currently declared and defined symbols so in a certain respect the
	exterior space data type replaces the traditional symbol table(s).

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
	// Represents any basis element from grade [0,n] in the currently declared space.
    grade:  int;
    bits:   int;
	symbol: string;
}

Factor :: struct {
	exponent: 			   s16;
	scalar_variable_index: u16;
}

Term :: struct {
	// Represents a single number or a list of scalar-variable factors to represent terms such as "a1" or a product like "a1b2c3"
	// This is like a tagged union without the tag. A number is implicit and occurs when the count of factors is zero.

	value: float;
	#place value; multiples: float;

	num_factors: u32;
	factors: [] Factor;
	next: *Term;
}

Scalar_Variables :: struct {
    hashes:  [..] u32;     
    symbols: [..] string;  // ex. "1", "a1", "b2", etc.
}

Exterior_Space :: struct {

	MAX_DIMENSION :: 6;
	MAX_NUMBER_OF_BASIS_ELEMENTS :: #run power(2, MAX_DIMENSION);

	dimension: int;
	n_element: *Basis_Element;

	basis_elements:         [MAX_NUMBER_OF_BASIS_ELEMENTS]  Basis_Element;
	bits_to_basis_elements: [MAX_NUMBER_OF_BASIS_ELEMENTS] *Basis_Element;

    scalar_variables: Scalar_Variables;
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

get_basis_element :: (bits: int) -> *Basis_Element {

	basis_element := space.bits_to_basis_elements[bits];
	return basis_element;
}

new_term :: (value: float) -> (term: *Term) {

	term: *Term = talloc(size_of(Term));
	assert(term != null);
	memset(term, 0, size_of(Term));

	term.value = value;
	return term;
}

new_term :: (multiples: float, exponent: s16, index: u16) -> (term: *Term) {

	term := cast(*Term)talloc(size_of(Term));
	assert(term != null);
	memset(term, 0, size_of(Term));

	term.factors.data  = talloc(size_of(Factor));
	term.factors.count = 1;
	assert(term.factors.data != null); 

	term.factors[0].exponent = exponent;
	term.factors[0].scalar_variable_index = index;
	term.num_factors   = 1;

	term.multiples = multiples;
	term.factors.count = 1;

	return term;
}

hash_scalar_variable :: (s: string) -> u32 {

	// Format of a scalar variable is an alphabet character followed by up to 7 digits.
	// The smallest variable is a0000000 and the largest is Z9999999.
	// The hashing algorithm (if you can even call it that) can fit all strings in this range in a u32.

    assert((s[0] >= #char "a" && s[0] <= #char "z") || (s[0] >= #char "A" && s[0] <= #char "Z"));
    assert(s.count <= 8);

    hash:   u64;

    powers: u64 = 1;
    digit:  u64 = 0;
	i := s.count-1;
    while i > 0 {
        digit  = s[i] - #char "0";
        digit  *= powers;
        hash  += digit;

        powers *= 10;
        
        i-= 1;
    }

    if s[0] >= #char "a" && s[0] <= #char "z" {
        digit  = s[i] - #char "a";
        digit *= powers;
        hash += digit;
    }
    else {
        digit  = (s[i] - #char "A") + 26;
        digit *= powers;
        hash += digit; 
    }

	U32_MAX :: 0xFFFF_FFFF;
    assert(hash <= U32_MAX);

    return cast(u32)hash;
}

insert_scalar_variable :: (scalar_variables: *Scalar_Variables, hash: u32, symbol: string) {

    // Big-O(n)
    found: bool = false;
    for i: 0..scalar_variables.hashes.count-1 {
        if hash == scalar_variables.hashes[i] {
            found = true;
            break;
        }
    }
    
    if !found {
        array_add(*scalar_variables.hashes,  hash);
        array_add(*scalar_variables.symbols, symbol);
    }

    return;
}

coefficient_to_string :: (term: *Term) -> string {

	builder: String_Builder;

	while term.next != null {
		if term.factors.count == 0 {
			value := tprint("%", term.value);
			append(*builder, value);
		}
		else {
			// scalar variable or product of scalar variables
			if term.multiples != 1 {
				multiples := tprint("%", term.multiples);
				append(*builder, multiples);
			}

			for i: 0..term.factors.count-1 {
				factor := term.factors[i];
				scalar_variable := space.scalar_variables.symbols[factor.scalar_variable_index];
				append(*builder, scalar_variable);

				if factor.exponent > 1 {
					exponent := tprint("%", factor.exponent);
					append(*builder,"^");
					append(*builder, exponent);
				}
			}
		}

		append(*builder, " + " );
	}

	// this is redundant but add the last term which is copy and pasting the code in the while loop above.
	if term.factors.count == 0 {
		value := tprint("%", term.value);
		append(*builder, value);
	}
	else {
		// scalar variable or product of scalar variables
		if term.multiples != 1 {
			multiples := tprint("%", term.multiples);
			append(*builder, multiples);
		}

		for i: 0..term.factors.count-1 {
			factor := term.factors[i];
			scalar_variable := space.scalar_variables.symbols[factor.scalar_variable_index];
			append(*builder, scalar_variable);

			if factor.exponent > 1 {
				exponent := tprint("%", factor.exponent);
				append(*builder,"^");
				append(*builder, exponent);
			}
		}
	}

	return builder_to_string(*builder);
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