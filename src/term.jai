/*
    A term represents several different forms of scalars.
    (1) A pure number like 3
    (2) A 'scalar-variable' like 'a1' which is a symbol representing a scalar in an expression.
    (3) A list of 'scalar-variables' such as "a1a2a3" which represents the product of 3 scalar variables "a1", "a2", and "a3".

    Generally, a term (which is implicitly a list of terms) is used as a coefficient for a basis element. When that basis element
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

    term.multiples = multiples;
    term.factors.count = 1;

    return term;
}

copy_terms :: (term: *Term) -> *Term {

    // Deep copy of the entire list of terms.
    assert(term != null);

    head := copy_term(term);
    
    src := term.next;
    dst := head;
    while src != null {
        dst.next = copy_term(src);
        dst = dst.next;
        src = src.next;
    }

    return head;
}

scale_term :: (term: *Term, s: float) {

    c := term;
    while c {
        c.multiples *= s;
        c = c.next;
    }

    return;
}

multiply_terms :: (lhs: *Term, rhs: *Term) -> *Term {

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

    head: Term;   // Hack: Don't return head, use it as a placeholder where the real 'head' is head.next
    Lterm := lhs;
    Rterm := rhs;

    current := *head;
    while Lterm != null {
        Rterm := rhs;
        while Rterm != null {
            current.next = multiply_term(Lterm, Rterm);
            current = current.next;
            Rterm = Rterm.next;
        }
        Lterm = Lterm.next;
    }

    return head.next;
}

term_to_string :: (term: *Term) -> string {

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

#scope_file

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