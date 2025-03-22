/*
    A term represents several different forms of scalars.
    (1) A pure number like 3
    (2) A 'scalar-variable' like 'a1' which is a symbol representing a scalar in an expression.
    (3) A list of 'scalar-variables' such as "a1a2a3" which represents the product of 3 scalar variables "a1", "a2", and "a3".

    A list of terms then represents sums of the above so something like "3 + a1 + b2c3"

    The list of terms is used as a coefficient for a basis element. When that basis element
    is grade-0 it represents the value of that 0-element.
*/

#scope_export

Factor :: struct {
    exponent: s16;
    scalar_variable: u16;
}

Term :: struct {
    value: float;
    #place value; multiples: float;

    factors: [] Factor;
    next: *Term;
}

List :: struct (T: Type) {
    // This list is not re-usable! Specific to terms!
    head: *T = null;
    tail: *T = null;
}

zero_terms :: (list: List(Term)) -> bool {
    return list.head == null && list.tail == null;
}

leading_term :: inline (list: List(Term)) -> *Term {
    return list.head;
}

last_term :: inline (list: List(Term)) -> *Term {
    return list.tail;
}

next_term :: inline (term: *Term) -> *Term {
    return term.next;
}

append_term :: (list: *List(Term), term: *Term) {
    
    // Adds the new to the end of the list.

    assert(list != null);
    assert(term != null);

    if zero_terms(list) {
        list.head = term;
        list.tail = term;
    }
    else {
        list.tail.next = term;
        list.tail      = term;
    }

    return;
}

new_coefficient :: (value: float) -> List(Term) {

    list: List(Term);
    term := new_term(value);

    append_term(*list, term);
    return list;
}

new_term :: (value: float) -> *Term {

    term: *Term = talloc(size_of(Term));
    assert(term != null);
    memset(term, 0, size_of(Term));

    term.value = value;
    return term;
}

new_coefficient :: (multiples: float, exponent: s16, scalar_variable: string) -> List(Term) {

    list: List(Term);
    term := new_term(multiples, exponent, scalar_variable);

    append_term(*list, term);
    return list;
}

new_term :: (multiples: float, exponent: s16, scalar_variable: string) -> *Term {

    term := cast(*Term)talloc(size_of(Term));
    assert(term != null);
    memset(term, 0, size_of(Term));

    term.factors.data  = talloc(size_of(Factor));
    term.factors.count = 1;
    assert(term.factors.data != null);

    term.factors[0].exponent        = exponent;
    term.factors[0].scalar_variable = encode(scalar_variable);

    term.multiples = multiples;
    term.factors.count = 1;

    return term;
}

copy_terms :: (source: List(Term)) -> List(Term), int {

    // Copy all the terms from the source list into the destination list.
    destination: List(Term);

    src := leading_term(source);
    append_term(*destination, copy_term(src));
    
    src = next_term(src);
    count := 1;
    while src != null {
        append_term(*destination, copy_term(src));
        src = next_term(src);

        count += 1;
    }

    return destination, count;
}

scale_coefficient :: (terms: List(Term), s: float) {

    c := leading_term(terms);
    while c {
        c.multiples *= s;
        c = c.next;
    }

    return;
}

multiply_coefficients :: (lhs: List(Term), rhs: List(Term)) -> List(Term) {

    /*
        Distribute scalar (and scalar variables).
        Cases:
        (1) LHS and RHS have an equal number of terms/coefficients:
            (a1)e1 ^ (b1)e2
        (2) LHS has more terms than RHS:
           (a1+a2)e1 ^ (b1)e2
        (3) Rhs has more terms than LHS:
           (a1)e1 ^ (b1+b2)e2

        Each element always has at *least* one term.
    */

    result: List(Term);
    Lterm := leading_term(lhs);
    Rterm := leading_term(rhs);

    while Lterm != null {
        while Rterm != null {
            product := multiply_term(Lterm, Rterm);
            append_term(*result, product);
            Rterm = next_term(Rterm);
        }
        Lterm = next_term(Lterm);
    }

    return result;
}

terms_are_equal :: (lterm: *Term, rterm: *Term) -> bool {
    
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

// TODO: Make sure this works with the example below!
combine_like_terms :: (lhs: List(Term), rhs: List(Term)) -> List(Term) {

    // Combines terms of common basis elements into a new list of terms.
    // Example: (3+a1)e1 + (b1+4+a1)e1 = (7+2a1+b1)e1

    result, count := copy_terms(lhs);

    print("Count == %\n", count);

    if zero_terms(rhs) { return result; }

    Rterm := leading_term(rhs);
    Lterm := leading_term(result);

    for i: 0..count-1 {
        while Rterm != null {
            if terms_are_equal(Lterm, Rterm) {
                // TODO: technically you don't need to iterate over this RHS again once a match is found but not worrying about it now.
                Lterm.multiples += Rterm.multiples;
            }
            else {
                // Cannot combine the two terms so add the rterm to the list of terms
                append_term(*result, Rterm);
            }

            Rterm = next_term(Rterm);
        }
        
        Lterm = next_term(Lterm);
    }

    return result;
 }

term_to_string :: (list: List(Term)) -> string {

    term_to_string_helper :: (term: *Term, builder: *String_Builder) {
        if term.factors.count == 0 {
            value := tprint("%", term.value);
            append(builder, value);
        }
        else {
            // scalar variable or product of scalar variables.
            if term.multiples != 1 {
                multiples := tprint("%", term.multiples);
                append(builder, multiples);
            }
            
            for i: 0..term.factors.count-1 {
                factor := term.factors[i];
                scalar_variable := decode(factor.scalar_variable);
                append(builder, scalar_variable);

                if factor.exponent > 1 {
                    exponent := tprint("%", factor.exponent);
                    append(builder,"^");
                    append(builder, exponent);
                }
            }
        }
    }

    builder: String_Builder;
    current := leading_term(list);
    
    while current.next != null {
        term_to_string_helper(current, *builder);
        append(*builder, " + ");
        current = current.next;
    }
    term_to_string_helper(current, *builder);

    return builder_to_string(*builder);
}

#scope_file

copy_term :: (term: *Term) -> *Term {

    // Copy of a single term.

    assert(term != null);

    src: *Term = term;
    dst: *Term = talloc(size_of(Term));
    assert(dst != null);

    memcpy(dst, src, size_of(Term));
    if src.factors.count > 0 {
        dst.factors.data  = talloc(src.factors.count * size_of(Factor));
        dst.factors.count = src.factors.count;
        assert(dst.factors.data != null);

        memcpy(dst.factors.data, src.factors.data, src.factors.count * size_of(Factor));
    }

    return dst;
}

term_is_number :: inline (term: *Term) -> bool { 
    return term.factors.count == 0; 
}

multiply_term :: (lhs: *Term, rhs: *Term) -> *Term {

    // Multiply single terms together.

    result: *Term = null;
    if term_is_number(lhs) {
        result = copy_term(rhs);
        result.multiples *= lhs.value;
    }
    else
    if term_is_number(rhs) {
        result = copy_term(lhs);
        result.multiples *= rhs.value;
    }
    else {
        result = new_term(lhs.multiples * rhs.multiples);
        result.factors = combine_factors(lhs.factors, rhs.factors);
    }

    assert(result != null);
    return result;
}

combine_factors :: (lhs: [] Factor, rhs: [] Factor) -> [] Factor {

    // Create a new combined list of sorted factors.
    // This is kind of like a sorted zip of the two lists.

    L := 0;
    R := 0;

    factor: Factor;
    factors: [..] Factor;

    while true {
        if L == lhs.count { // no more factors in the left factor list; add the rest of the right
            while R < rhs.count {
                array_add(*factors, rhs[R]);
                R += 1;
            }
            break;
        }

        if R == rhs.count { // no more factors in the right factor list; add the rest of the left
            while L < lhs.count {
                array_add(*factors, lhs[L]);
                L += 1;
            }
            break;
        }

        if lhs[L].scalar_variable == rhs[R].scalar_variable {
            factor           = lhs[L];
            factor.exponent *= rhs[R].exponent;

            array_add(*factors, factor);
            L += 1;
            R += 1;
        }
        else
        if lhs[L].scalar_variable < rhs[R].scalar_variable {
            array_add(*factors, lhs[L]);
            L += 1;
        }
        else {
            array_add(*factors, rhs[R]);
            R += 1;
        }
    }

    return factors;
}