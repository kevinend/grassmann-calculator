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

    factors:      [] Factor; // TODO: Remove!
    numerators:   [] Factor;
    denominators: [] Factor;
    count:   int;
}

Coefficient :: struct {
    terms: [] Term;
}

// Zero and one are used frequently. Instead of constantly allocating set once and reuse.
term_zero :: Term.{ value = 0 };
term_one  :: Term.{ value = 1 };

coefficient_zero: Coefficient;
coefficient_one:  Coefficient;

initialize_coefficients :: () {
    coefficient_zero.terms.data  = *term_zero;
    coefficient_zero.terms.count = 1;
    
    coefficient_one.terms.data = *term_one;
    coefficient_one.terms.count = 1;

    return;
}

ZERO :: () -> Coefficient {
    return coefficient_zero;
}

ONE :: () -> Coefficient {
    return coefficient_one;
} 

coefficient_contains_no_terms :: inline (terms: [] Term) -> bool {
    return terms.count == 0;
}

remove_term :: inline (terms: *[] Term, index: int) {

    // Swap the term to remove with the end and decrement the count.
    // This reorders the terms but doesn't impact the result. Factors have to stay in order but terms don't.
    assert(terms.count > 0);

    terms[index] = terms[terms.count-1];
    terms.count -= 1;

    return;
}

new_coefficient :: (value: float) -> [] Term {

    // TODO: Make this a coefficient that refers to other terms! coefficient.terms.data 
    terms: [] Term;
    terms.data = alloc(1*size_of(Term));
    terms.count = 1;

    terms[0] = new_term(value);
    return terms;
}

new_term :: (value: float) -> Term {

    term: Term;
    term.value = value;
    return term;
}

new_coefficient :: (multiples: float, exponent: s16, scalar_variable: string) -> [] Term {

    terms: [] Term;
    terms.data = alloc(1*size_of(Term));
    terms.count = 1;

    terms[0] = new_term(multiples, exponent, scalar_variable);
    return terms;
}

new_term :: (multiples: float, exponent: s16, scalar_variable: string) -> Term {

    term: Term;

    term.multiples = multiples;

    term.factors.data  = alloc(size_of(Factor));
    term.factors.count = 1;
    assert(term.factors.data != null);

    term.factors[0].exponent        = exponent;
    term.factors[0].scalar_variable = encode(scalar_variable);
    term.factors.count = 1;

    return term;
}

deep_copy_terms :: (source: [] Term) -> [] Term {

    // Copy all the terms from the source list into the destination list.
    destination: [] Term;
    destination.data  = alloc(source.count * size_of(Term));
    destination.count = source.count; 
    assert(destination.data != null);

    memcpy(destination.data, source.data, source.count * size_of(Term));

    return destination;
}

free_term :: (source: *Term) {

    assert(source != null);

    if source.factors.count > 0 {
        free(source.factors.data);
    }
    free(source);

    return;
}

free_terms :: (terms: [] Term) {

    // Deallocate buffer storing the terms.
    free(terms.data);
    return;
}

scale_coefficient :: (terms: [] Term, s: float) {

    for *term: terms {
        term.multiples *= s;
    }
}

multiply_coefficients :: (lhs: [] Term, rhs: [] Term) -> [] Term {

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

    // TODO: We should copy LHS and RHS into a separate scratch buffer!

    scratch: [..] Term;

    for i: 0..lhs.count-1 {
        for j: 0..rhs.count-1 {
            product := multiply_term(*lhs[i], *rhs[j]);
            array_add(*scratch, product);
        }
    }

    result: [] Term;
    result = scratch;
    return result;
}

terms_are_equal :: (lterm: Term, rterm: Term) -> bool {
    
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

combine_like_terms :: (lhs: [] Term, rhs: [] Term) -> [] Term {

    // TODO: Use the term/coefficient scratch buffer for this

    // Combines terms of common basis elements into a new list of terms.
    // Example: (3+a1)e1 + (b1+4+a1)e1 = (7+2a1+b1)e1
    // Input:   (3*e1 + a1*e1) + (b1*e1 + 4*e1 + a1*e1)
    // @WASTE: There is a lot of unnecessary copying going on here...

    // Allocate worst-case space is that none of the terms can be combined.
    size := lhs.count + rhs.count;
    result: [] Term;
    result.data = alloc(size * size_of(Term));
    result.count = lhs.count + rhs.count;
    assert(result.data != null);

    Lterms := deep_copy_terms(lhs);
    Rterms := deep_copy_terms(rhs);

    k := 0;
    for i: 0..Lterms.count-1 {
        for j: 0..Rterms.count-1 {
            if terms_are_equal(Lterms[i], Rterms[j]) {
                Lterms[i].multiples += Rterms[j].multiples;
                result[k] = Lterms[i];
                k += 1;

                remove_term(*Rterms, j);
            
            }
        }
    }

    // append the remaining elements from the RHS that could *not* be combined with the LHS.
    // TODO: deep_copy_terms, deep_copy_term (for individual term)
    for j: 0..Rterms.count-1 {
        result[k] = Rterms[j];
        k += 1;
    }

    return result;
 }

coefficient_to_string :: (terms: [] Term) -> string {

    term_to_string :: (term: *Term, builder: *String_Builder) {
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
    for i: 0..terms.count-2 {
        term_to_string(*terms[i], *builder);
        append(*builder, "+");
    }
    term_to_string(*terms[terms.count-1], *builder);

    return builder_to_string(*builder);
}

#scope_file


term_is_number :: inline (term: *Term) -> bool { 
    return term.factors.count == 0; 
}

multiply_term :: (lhs: *Term, rhs: *Term) -> Term {

    // Multiply single terms together.

    result: Term;
    if term_is_number(lhs) {
        result = rhs;                   // TODO: Maybe shallow copy isn't right?
        result.multiples *= lhs.value;
    }
    else
    if term_is_number(rhs) {
        result = lhs;                   // TODO: Maybe shallow copy isn't right?
        result.multiples *= rhs.value;
    }
    else {
        result = new_term(lhs.multiples * rhs.multiples);
        result.factors = combine_factors(lhs.factors, rhs.factors);
    }

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