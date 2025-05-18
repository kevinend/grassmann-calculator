#scope_export

Coefficient :: struct {

    first_term: Relative_Pointer(Term);
    last_term:  Relative_Pointer(Term);
    term_count: s32;  // TODO: metaprogram to ensure anything that is a count is a SIGNED type to prevent underflow in loops
}


// Procedures used in creation and management of new coefficients.
new_coefficient :: (value: float) -> *Coefficient {

    coefficient := cast(*Coefficient)heap_allocate(context.expression_heap, size=size_of(Coefficient), alignment=size_of(Coefficient));
    assert(coefficient != null);
    
    term := new_term(value);
    append_term(term, coefficient);

    return coefficient;
}

new_coefficient :: (multiples: float, factor_type: Factor_Type, exponent: u16, scalar_variable: string) -> *Coefficient {

    coefficient := cast(*Coefficient)heap_allocate(context.expression_heap, size=size_of(Coefficient), alignment=size_of(Coefficient));
    assert(coefficient != null);
    memset(coefficient, 0, size_of(Coefficient));

    term   := new_term(multiples);
    factor := new_factor(exponent, scalar_variable);

    append_term(term, coefficient);
    append_factor(factor, term);

    return coefficient;
}

clear_coefficient :: (coefficient: *Coefficient) {

    // reset the coefficient to be 'empty'
    // do *not* memset to zero or adjust .heap field manually!

    coefficient.term_count = 0;
    set(*coefficient.first_term, null);
    set(*coefficient.last_term,  null);

    return;
}

get_coefficient_size :: (coefficient: *Coefficient) -> int {

    start := cast(*u8)coefficient;
    last  := get(coefficient.last_term);
    end   := cast(*u8)(get(last.factors) + last.factor_count);

    return end - start;
}

coefficient_to_string :: (coefficient: *Coefficient) -> string {

    term_to_string :: (term: *Term, builder: *String_Builder) {
        if term.factor_count == 0 {
            value := tprint("%", term.value);
            append(builder, value);
        }
        else {
            // scalar variable or product of scalar variables.
            if term.multiples != 1 {
                multiples := tprint("%", term.multiples);
                append(builder, multiples);
            }

            factors := get(term.factors);
            for i: 0..term.factor_count-1 { // TODO: 04.26.2025 this is obviously wrong!
                // Assumes the factors are ordered!!! Numerators following denominators!
                if factors[i].type == .Numerator {
                    numerator := factors[i];
                    scalar_variable := decode(numerator.scalar_variable);
                    append(builder, scalar_variable);

                    if numerator.exponent > 1 {
                        exponent := tprint("%", numerator.exponent);
                        append(builder,"^");
                        append(builder, exponent);
                    }
                }
                else
                if factors[i].type == .Denominator {
                    denominator := factors[i];
                    scalar_variable := decode(denominator.scalar_variable);
                    append(builder, scalar_variable);
                    append(builder, "__");

                    if denominator.exponent > 1 {
                        exponent := tprint("%", denominator.exponent);
                        append(builder,"^");
                        append(builder, exponent);
                    }
                }
            }
        }
    }

    builder: String_Builder;
    count := coefficient.term_count;
    term  := get(coefficient.first_term);
    for i: 0..count-2 {
        term_to_string(term, *builder);
        append(*builder, "+");
        term = get_next_term(term);
    }
    term_to_string(term, *builder);

    return builder_to_string(*builder);
}

append_term :: (term: *Term, coefficient: *Coefficient) {

    if coefficient.term_count == 0 {
        set(*coefficient.first_term, term);
        set(*coefficient.last_term,  term);
    }
    else {
        last := get(coefficient.last_term);
        set_next_term    (last, term);
        set_previous_term(term, last);
        set_next_term    (term, null);
        
        set(*coefficient.last_term, term);
    }

    coefficient.term_count += 1;
    return;
}

remove_term :: (term: *Term, coefficient: *Coefficient) {

    // Terms are a doubly linked list; adjust pointers.

    first_term := get(coefficient.first_term);
    last_term  := get(coefficient.last_term);
    if first_term == last_term {
        // single element in the 'list'
        set(*coefficient.first_term, null);
        set(*coefficient.last_term,  null);
    }
    else
    if term == first_term {
        // adjust the head
        next := get_next_term(term);
        set(*coefficient.first_term, next);
    }
    else 
    if term == last_term {
        previous := get_previous_term(term);
        set(*coefficient.last_term, previous);
    }
    else {
        previous := get_previous_term(term);
        next     := get_next_term(term);

        set_next_term(previous, next);
        set_previous_term(next, previous);
    }

    coefficient.term_count -= 1;
    return;
}

#scope_file

// The coefficient heap stores all coefficients, terms and factors for all nodes in the AST prior to evaluation.
// During evaluation data from this heap is copied into the working set for evaluation purposes which requires mutating terms and factors.
// Once evaluation is complete the heap is reset to be available to work on the next expression.
default_coefficient_heap: Bump_Heap;

// Provide canonical representations for the coefficients zero (0) and one (1).
__zero :: *Coefficient;
__one  :: *Coefficient;
