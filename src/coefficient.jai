/*
    Each basis element has a coefficient which represents the 'value' of that particular basis element.
    A coefficient is an instrusive list of terms.
    Each term represents one of several different kinds of scalar values.
        1. Pure numbers like 0, 1, 2, etc..
        2. Scalar variables such as 'a1','b1','c1' which is a text symbol representing an arbitrary scalar in an expression.
        3. Lists of scalar variables such as 'a1b1c1' which is the product of 3 scalar variables 'a1','b1', and 'c1.

    Terms are 'tightly-packed'.
    A term is followed immediately by its list of numerators then its list of denominators in that order.

    This was done for several reasons:
        1. The original implementation used the array-slice mechanism but the numerators and denominators took up 32-bytes collectively
           just for metadata about the lists that immediately followed the term anyways.
        2. Unlikely that any individual term in a coefficient needs more than 127 (s8) numerators or denominators.
        3. Having term be only 8 bytes makes pure number terms like 0,1,2,3 only double the cost rather than 16 times (the Term was once 64 bytes) the size.
        4. Having the relative pointer mechanism in the term also allows for easier serialization/deserialization to disk.
*/

#scope_export

Factor :: struct {
    exponent: s16;
    scalar_variable: u16;
}

Numerator   :: Factor;
Denominator :: Factor;

Term :: struct {
    value: float;
    #place value; multiples: float;

    // .counts are signed to prevent underflow when using 'for i: 0..count-1' when numerators or denominators are zero.
    numerator_count:   s8 = 0;
    denominator_count: s8 = 0;
    next: Relative_Pointer(Term);
}

Coefficient :: struct {
    term_count: int;
    terms:      *Term;
}

// Zero and one are used frequently. Instead of constantly allocating set once and reuse.
term_zero :: Term.{ value = 0 };
term_one  :: Term.{ value = 1 };

coefficient_zero: Coefficient;
coefficient_one:  Coefficient;

initialize_coefficients :: () {

    coefficient_zero.terms = *term_zero;
    coefficient_zero.term_count = 1;

    coefficient_one.terms = *term_one;
    coefficient_one.term_count = 1;

    return;
}

zero :: () -> Coefficient {
    return coefficient_zero;
}

one :: () -> Coefficient {
    return coefficient_one;
}

get_numerators :: (term: *Term) -> [] Numerator {

    numerators: [] Numerator;
    numerators.data = cast(*Numerator)(cast(*u8)term + size_of(Term));
    numerators.count = term.numerator_count;

    return numerators;
}

get_denominators :: (term: *Term) -> [] Denominator {

    denominators: [] Denominator;
    denominators.data  = cast(*Denominator)(cast(*u8)term + size_of(Term) + (term.numerator_count * size_of(Numerator)));
    denominators.count = term.denominator_count;

    return denominators; 
}

// TODO: 04.06.2025 haven't checked literally anything under here yet!


number_of_terms :: inline (coefficient: Coefficient) -> int {
    return coefficient.term_count;
}

remove_term :: inline (coefficient: *Coefficient, index: int) { // TODO: 04.09.2025 Refactoring? needed???

    // Swap the term to remove with the end and decrement the count.
    // This reorders the terms but doesn't impact the result. Factors have to stay in order but terms don't.

    assert(coefficient.term_count > 0);

    if index != coefficient.term_count-1 && coefficient.term_count != 1 {
        // Swap terms if you aren't the last term or the number of terms isn't one.
   //     coefficient.terms[index] = coefficient.terms[coefficient.term_count-1];
    }

    coefficient.term_count -= 1;
    return;
}

new_coefficient :: (value: float) -> Coefficient {

    coefficient: Coefficient;

    coefficient.terms = new_term(value);
    coefficient.term_count = 1;
    return coefficient;
}

new_term :: (value: float) -> *Term {

    term := cast(*Term)alloc(1*size_of(Term));
    assert(term != null);

    term.value = value;
    return term;
}

new_coefficient :: (multiples: float, exponent: s16, scalar_variable: string) -> Coefficient {

    coefficient: Coefficient;

    coefficient.terms = new_term(multiples, exponent, scalar_variable);
    coefficient.term_count = 1;
    return coefficient;
}

// TODO: Allocate in our 'Coefficient' buffer instead of temp!! 04.08.2025
new_term :: (multiples: float, exponent: s16, scalar_variable: string) -> *Term {

    memory: *u8 = alloc(size_of(Term) + size_of(Numerator));
    assert(memory != null);

    term := cast(*Term)memory;
    term.multiples = multiples;

    numerator := cast(*Numerator)(memory + size_of(Term));
    numerator.exponent = exponent;
    numerator.scalar_variable = encode(scalar_variable);

    return term;
}

free_term :: (source: *Term) { // TODO: I'm not sure this is going to be a thing, just free the whole coefficient

    assert(source != null);

    if source.numerator_count > 0 {
       // free(source.factors.data); // TODO: 04.09.2025 Refactoring!
    }
   // free(source); // TODO: 04.09.2025 Refactoring!

    return;
}

free_coefficient :: (coefficient: Coefficient) {

    // Deallocate buffer storing the terms.
    // TODO: Should this free the numerators and denominators?
    // TODO: 04.09.2025 -- Does nothing right now!
   // free(coefficient.terms.data);
    return;
}

scale_coefficient :: (coefficient: Coefficient, s: float) {

/* TODO: Replace this with some traversal over the terms to scale each one of them.
    for *term: coefficient.terms {
        term.multiples *= s;
    }
*/
}

multiply_coefficients :: (lhs: Coefficient, rhs: Coefficient) -> Coefficient {

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

    for i: 0..lhs.term_count-1 {
        for j: 0..rhs.term_count-1 {
            product := multiply_term(*lhs.terms[i], *rhs.terms[j]);
            array_add(*scratch, product);
        }
    }

    result: Coefficient;
  //  result.terms = scratch; // TODO: 04.09.2025 Refactoring!
    return result;
}

terms_are_equal :: (lterm: Term, rterm: Term) -> bool {

	// Terms are equal when the following conditions are true.
	//  1. Each term contains the same number of factors (even if the count is zero).
	//  2. Each term contains the same factors in the same order.
	//  3. Each factor in the term is raised to the same power.
	if lterm.numerator_count == rterm.numerator_count {
		count := lterm.numerator_count;
        return true; // TODO: This is obviously wrong! 04.09.2025
        //	return memcmp(lterm.factors.data, rterm.factors.data, size_of(Factor) * count) == 0;
	}
	else {
		return false;
	}
}

append_term :: (coefficient: *Coefficient, src: *Term) #no_abc { // TODO: 04.09.2025 Refactoring. needed???

    // Add the term to the list of terms belonging to the coefficient.
    // Coefficient *must* belong to the scratch space!
    //
    // #no_abc is set to handle when the coefficient has zero terms.
    // Each coefficient in scratch space points to valid memory but the term_count == 0 so you can't index the array view without the #no_abc.

    assert(coefficient.term_count < MAX_TERMS_PER_COEFFICIENT);

    i   :=  coefficient.term_count;
    dst := *coefficient.terms[i];

    memcpy(dst, src, size_of(Term));

    // Copy the term's factors and denominators.
  //  memcpy(dst.numerators.data,   src.numerators.data,   src.numerators.count   * size_of(Factor));
   // memcpy(dst.denominators.data, src.denominators.data, src.denominators.count * size_of(Factor));

    dst.numerator_count   = src.numerator_count;
    dst.denominator_count = src.denominator_count;

    coefficient.term_count += 1;
    return;
}

sum_coefficients :: (lhs: *Coefficient, rhs: *Coefficient) {

    // The LHS comes from the scratch space for the current basis element.
    // The RHS is copied into the working set space for the current calculation.
    // Combine the results from the LHS with the RHS by updating and appending to the LHS in the scratch space.
    //
    // Since RHS was copied into the working set memory you *can* mutate it without impacting the original coefficient
    // from the AST.

    for i: 0..lhs.term_count-1 {
        for j: 0..rhs.term_count-1 {
            if terms_are_equal(lhs.terms[i], rhs.terms[j]) {
                lhs.terms[i].multiples += rhs.terms[j].multiples;
                remove_term(rhs, j);
            }
        }
    }

    // Append the remaining terms from the RHS that could *not* be combined with the LHS.
    assert((lhs.term_count + rhs.term_count) <= MAX_TERMS_PER_COEFFICIENT);

    for j: 0..rhs.term_count-1 {
        append_term(lhs, *rhs.terms[j]);
    }
}

coefficient_to_string :: (coefficient: Coefficient) -> string {

    term_to_string :: (term: *Term, builder: *String_Builder) {
        if term.numerator_count == 0 {
            value := tprint("%", term.value);
            append(builder, value);
        }
        else {
            // scalar variable or product of scalar variables.
            if term.multiples != 1 {
                multiples := tprint("%", term.multiples);
                append(builder, multiples);
            }

            numerators := get_numerators(term);
            for i: 0..numerators.count-1 {
                numerator := numerators[i];
                scalar_variable := decode(numerator.scalar_variable);
                append(builder, scalar_variable);

                if numerator.exponent > 1 {
                    exponent := tprint("%", numerator.exponent);
                    append(builder,"^");
                    append(builder, exponent);
                }
            }
        }
    }

    builder: String_Builder;
    count := coefficient.term_count;
    for i: 0..count-2 {
        term_to_string(*coefficient.terms[i], *builder);
        append(*builder, "+");
    }
    term_to_string(*coefficient.terms[count-1], *builder);

    return builder_to_string(*builder);
}

#scope_file


term_is_number :: inline (term: *Term) -> bool {
    return term.numerator_count == 0;
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
      //  result.factors = combine_factors(lhs.factors, rhs.factors);
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