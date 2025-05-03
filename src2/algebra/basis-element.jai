#scope_export

Basis_Bitset :: int;

Basis_Element :: struct {
    // Immutable defintion of a basis element from grade [0,n] in the currently declared space.
    grade:  int;
    bitset: Basis_Bitset;
    symbol: string;
}

generate_basis_elements :: (dimension: int, _1_element_symbols: [] string) -> [] Basis_Element, string {

    // Generates all basis elements from 0 to N belonging to a space with a particular dimension.
    // Generating the basis elements happens in two steps:
    // 1. Create each basis element with its grade and bitset
    // 2. Create a symbol for each element and associate it to the correct basis element created in step 1.

    num_basis_elements := power(2, dimension);
    basis_elements: [..] Basis_Element;
    array_reserve(*basis_elements, num_basis_elements);

    for i: 0..dimension {
        populate_basis_elements_of_grade_k(
             k=i
            ,n=dimension
            ,c=0
            ,grade=0
            ,bitset=0
            ,elements=*basis_elements
        );
    }

    delimiter: u8 = #char "|";

    basis_symbol_builder: String_Builder;
    for i: 0..basis_elements.count-1 {
        build_and_append_basis_symbol(basis_elements[i], _1_element_symbols, *basis_symbol_builder, delimiter);
    }
    basis_symbols := builder_to_string(*basis_symbol_builder);

    cursor: *u8 = *basis_symbols[0];
    for i: 0..basis_elements.count-1 {
        basis_elements[i].symbol = get_next_basis_symbol(*cursor, delimiter);
    }

    return basis_elements, basis_symbols;
}

generate_bitset_to_basis_element_map :: (basis_elements: [] Basis_Element) -> [] *Basis_Element {

    bitset_to_basis_elements: [] *Basis_Element;
    bitset_to_basis_elements.data = alloc(basis_elements.count * size_of(*Basis_Element));
    assert(bitset_to_basis_elements.data != null);
    bitset_to_basis_elements.count = basis_elements.count;

    for i: 0..basis_elements.count-1 {
        index := basis_elements[i].bitset;
        bitset_to_basis_elements[index] = *basis_elements[i];
    }

    return bitset_to_basis_elements;
}

#scope_file
#import "Basic";

power :: (a: int, b: int) -> int {

    // Implements the basic arithment expression: c = a^b
    c: int = 1;
    for i: 0..b-1 {
        c *= a;
    }
    return c;
}

populate_basis_elements_of_grade_k :: (
     k:            int   // target grade
    ,n:            int   // dimension of the space; max number of 1-elements belonging to the space
    ,c:            int   // current 1-element = (1 << c)
    ,grade:        int   // current grade of basis element  being constructed
    ,bitset:       int   // current bitset of basis element being constructed
    ,elements:    *[..] Basis_Element
) {

    // Generates all basis elements of grade 'k' from a given space and adds them to a collection provided by the caller.
    // Example: _1_elements_symbols = .["e0","e1","e2","e3"]; 1-elements are represented as individual bits (1 << 1) == e1 in this case.
    //          Since there are 4, 1-elements then the dimension of the space is 4 (n=4)
    //
    //          populate_basis_elements_of_grade_k(
    //                 k = 2
    //                 n = 4
    //                 c = 0
    //                 grade = 0
    //                 bitset = 0
    //                 elements = *[..] ; empty starting list
    //          )
    //      
    //          elements = [
    //              .{ grade = 2; bitset = 3 }  // e0^e1
    //              .{ grade = 2; bitset = 5 }  // e0^e2
    //              .{ grade = 2; bitset = 9 }  // e0^e3
    //              .{ grade = 2; bitset = 6 }  // e1^e2
    //              .{ grade = 2; bitset = 10 } // e1^e3
    //              .{ grade = 2; bitset = 12 } // e2^e3
    //          ]
    //
    // When executed in a loop from 0..N with the same buffer ('elements') in this case. We generate all the basis elements
    // in the space up to the dimension in order of 0-elements, then 1-elements, then 2-elements, etc..

    if grade == k {
        // found a basis element of grade k, add to our collection of basis elements
        element := Basis_Element.{ grade = grade, bitset = bitset };
        array_add(elements, element);
        return;
    }

    if c >= n {
        // reached the end of the 1-elements without generating an element of grade k
        return;
    }

    populate_basis_elements_of_grade_k(k, n, c+1, grade+1, bitset | (1<<c), elements); // include current element
    populate_basis_elements_of_grade_k(k, n, c+1, grade,   bitset,          elements); // exclude current element
}

build_and_append_basis_symbol :: (element: Basis_Element, _1_element_symbols: [] string, builder: *String_Builder, delimiter: u8) {

    if element.grade == 0 {
        append(builder, "1");
    }
    else {
        wedge := "^";
        i     := 0;
        k     := 0;

        while true {
            if element.bitset & (1 << i) {
                if k == element.grade-1 { break; }
                else {
                    append(builder, _1_element_symbols[i]);
                    append(builder, wedge);
                    k += 1;
                }
            }
            i += 1;
        }
        append(builder, _1_element_symbols[i]);
    }
    append(builder, delimiter);
    return;
}

get_next_basis_symbol :: (cursor: **u8, delimiter: u8) -> string {

    symbol: string;
    symbol.data = cursor.*;

    count := 0;
    while cursor.*.* != delimiter {
        count    += 1;
        cursor.* += 1;
    }

    // advance past delimiter that we terminated on above.
    cursor.* += 1;

    symbol.count = count;
    return symbol;
}