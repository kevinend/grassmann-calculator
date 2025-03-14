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
    exponent: s16;
    scalar_variable: u16;
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

M_Element :: struct {
    // An M-element represents a multi-element. An element that *can* be of mixed grade.
    grade: int      = -1;
    ast:  *Ast_Node = null;
}

Exterior_Space :: struct {

    MAX_DIMENSION :: 6;
    MAX_NUMBER_OF_BASIS_ELEMENTS :: #run power(2, MAX_DIMENSION);

    dimension: int;
    n_element: *Basis_Element;

    basis_elements:         [MAX_NUMBER_OF_BASIS_ELEMENTS]  Basis_Element;
    bits_to_basis_elements: [MAX_NUMBER_OF_BASIS_ELEMENTS] *Basis_Element;

    scalar_variable_prefix_characters: string;
    user_variables: Table(string, M_Element);
}

space: Exterior_Space;

update_exterior_space :: (dimension: int, _1_element_symbols: [] string, scalar_variable_prefix_characters: string) {

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

    space.scalar_variable_prefix_characters = scalar_variable_prefix_characters;
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

new_term :: (multiples: float, exponent: s16, scalar_variable: string) -> (term: *Term) {

    term := cast(*Term)talloc(size_of(Term));
    assert(term != null);
    memset(term, 0, size_of(Term));

    term.factors.data  = talloc(size_of(Factor));
    term.factors.count = 1;
    assert(term.factors.data != null);

    term.factors[0].exponent        = exponent;
    term.factors[0].scalar_variable = encode(scalar_variable);
    term.num_factors = 1;

    term.multiples = multiples;
    term.factors.count = 1;

    return term;
}

operator == :: (lterm: Term, rterm: Term) -> bool {
    
	// Terms are equal when the following conditions are true.
	//  1. Each term contains the same number of factors (even if the count is zero).
	//  2. Each term contains the same factors in the same order.
	//  3. Each factor in the term is raised to the same power.
	if lterm.factors.count == rterm.factors.count {
		count := lterm.factors.count;

		return memcmp(lterm.factors.data, rterm.factors.data, size_of(Factor) * count) == 0;
	}
	else {
		return false;
	}
}


/* 
STUB:
combine_like_terms :: (lhs: *Term, rhs: *Term) -> *Term {
 
	// Example:
	// Input  = (3 + a1) + (b1 + 4 + a1)
	// Output = (7 + 2a1 + b1)
	Copy the lhs terms into a new list. Keep track of the number of terms.
	Iterate over each term in the copy.
	  -- Iterate by index from previous step
	  -- If you iterate while .next != null then when you append to your copy from the rhs you will unnecessarily iterate that element over the rest of the rhs)
	  -- Double-check ^ might not be true, too tired.
	  Iterate over each term in the rhs.
		if lterm == rterm {
			lterm-copy.multiples = lterm.multiples + rterm.multiples;
			-- can keep track of the previous while iterating rhs and 'remove' this node from the rhs so it isn't constantly checked
		}
		else {
			-- can't combine the two terms so add the rterm to the list of lterms
		}
 }
 
*/
 

matches_1_element :: (ident: string) -> *Basis_Element {

    element: *Basis_Element = null;
    for i: 1..space.dimension {
        if ident == space.basis_elements[i].symbol {
            element = *space.basis_elements[i];
            break;
        }
    }

    return element;
}

match_scalar_variable :: (ident: string) -> bool {

    // Scalar variables are identifiers that have a leading character in the 'allowed-list' for scalar variables and are followed by at least 1 digit.
    matching_character_prefix: bool = false;
    for i: 0..space.scalar_variable_prefix_characters.count-1 {
        if ident[0] == space.scalar_variable_prefix_characters[i] {
            matching_character_prefix = true;
            break;
        }
    }

    followed_by_digit: bool = false;
    if ident.count > 1 && is_digit(ident[1]) {
        followed_by_digit = true;
    }

    return matching_character_prefix && followed_by_digit;
}

valid_scalar_variable :: (ident: string) -> bool {

    // Scalar variables are identifiers that have a leading character in the 'allowed-list' for scalar variables and be followed by at *most* two digits.
    return ident.count <= 3;
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
                scalar_variable := decode(factor.scalar_variable);
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
            scalar_variable := decode(factor.scalar_variable);
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

lookup_variable :: (ident: string) -> bool, M_Element {

    variable, found := table_find(*space.user_variables, ident);
    return found, variable;
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

/*

Encode and decode scalar variables into u16's so that we don't have to store the text string to represent the variable.
This allows 4 bytes to be used (the encoding + exponent) to represent any *valid* scalar variable.
When a term contains a product (list) of scalar-variables this keeps the size of that product relatively small.
Also allows factors to be ordered based on the u16 key which makes memcmp an available option to compare terms easily
vs iterating over terms then iterating over liss of factors.

An alternative approach is to intern the scalar variable strings. Then to compare two factors you just compare the pointers.
Since we have to store the exponent alongside the pointer this makes the size of each factor grow to 16-bytes and we can't
sort by pointer so the check would be an iteration over each list of factors.

A third approach would be to maintain two parallel lists for scalar variables. A list of hashes and a list of symbols.
When you encounter a scalar-variable you hash it (using some stupid simple hash that does powers of ten on place-value or something)
then iterate through your list and look for that hash. If you find it then you return the index otherwise you append the list
with the new hash and symbol and store the index in the Factor. If you allow fewer than 65535 scalar-variables the index could
fit in a u16 and be used in the same manner as our current encode/decode scheme. The drawback to this approach is if the application
is long-running a lot of scalar-variables could exist in the parallel lists and those checks just take longer and longer to execute.
Could do an ordered insert of the hashes and symbols then binary search on lookup but that complicates the insert to ease the calc and lookup....

The most tedious part when it comes to scalar-variables is by far checking for equal terms so going to go with the u16 encoding for now
and if it is problematic then we revise. 03.12.2025

*/

encode :: (ident: string) -> u16 {

    // Encodes a scalar variable like 'a1' or 'a11' into a 16-bit integer in the following format:
    //                 byte1                byte2
    //       -----------------------| ---------------
    // u16 = [character  num-digits | digit1  digit2]
    //        ^6-bits    ^2-bits     ^4-bits  ^4-bits
    //

    assert(ident.count == 2 || ident.count == 3);

    c := cast(u16)(ident[0] - #char "A"); // A = 65, z = = 122, diff = 58 which can be represented in 6 bits
    n := cast(u16)ident.count - 1;        // n is the number of digits following the character.

    value: u16;
    value |= c << 10;
    value |= n <<  8;

    if n == 2 {
        value |= cast(u16)(ident[1] - #char "0") << 4;
        value |= cast(u16)(ident[2] - #char "0") << 0;
    }
    else
    if n == 1 {
        value |= cast(u16)(ident[1] - #char "0") << 4;
    }

    return value;
}

decoder: String_Builder;
decode :: (value: u16) -> string {

    // Decodes a 16-bit integer like 1296 into the scalar symbol 'B1'
    // Encoding procedure describes the format of the encoding.

    reset(*decoder);
    append(*decoder, cast(u8)((value >> 10) & 0x3F) + #char "A");

    count := cast(u8)((value >> 8) & 0x03);

    if count == 2 {
        append(*decoder, cast(u8)((value >> 4) & 0xF) + #char "0");
        append(*decoder, cast(u8)((value >> 0) & 0xF) + #char "0");
    }
    else
    if count == 1 {
        append(*decoder, cast(u8)((value >> 4) & 0xF) + #char "0");
    }

    return builder_to_string(*decoder);
}