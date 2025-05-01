#scope_export

Coefficient :: struct {
    heap:       *Bump_Heap;
    first_term: Relative_Pointer(Term);
    last_term:  Relative_Pointer(Term);

    term_count: s32;  // TODO: metaprogram to ensure anything that is a count is a SIGNED type to prevent underflow in loops
}

zero :: () -> *Coefficient { return __zero; }
one  :: () -> *Coefficient { return __one;  }

initialize_default_coefficient_heap :: () {

    // Initialize coefficients that are used constantly for expressions, namely zero and one.
    __zero = new_coefficient(0);
    __one  = new_coefficient(1);

    // Set checkpoint so that when we reset we leave zero and one untouched for subsequent executions.
    heap_set_checkpoint(*default_coefficient_heap);

    return;
}

reset_default_coefficient_heap :: () {

    heap_reset_to_checkpoint(*default_coefficient_heap);
    return;
}

new_coefficient :: (value: float, heap: *Bump_Heap = null) -> *Coefficient {

    coefficient: *Coefficient = null;
    if !heap {
        coefficient = allocate_coefficient(*default_coefficient_heap);
    }
    else {
        coefficient = allocate_coefficient(heap);
    }
    
    term := new_term(value, coefficient.heap);
    append_term(term, coefficient);

    return coefficient;
}

new_coefficient :: (multiples: float, factor_type: Factor_Type, exponent: u16, scalar_variable: string, heap: *Bump_Heap = null) -> *Coefficient {

    coefficient: *Coefficient;
    if !heap {
        coefficient := allocate_coefficient(*default_coefficient_heap);
    }
    else {
        coefficient := allocate_coefficient(heap);
    }

    term   := new_term(multiples, coefficient.heap);
    factor := new_factor(factor_type, exponent, scalar_variable, coefficient.heap);

    append_term(term, coefficient);
    append_factor(factor, term);

    return coefficient;
}

allocate_coefficient :: (heap: *Bump_Heap) -> *Coefficient {
    
    coefficient := cast(*Coefficient)heap_allocate(heap, size = size_of(Coefficient), alignment = size_of(Coefficient));
    assert(coefficient != null);
    memset(coefficient, 0, size_of(Coefficient));

    coefficient.heap = heap;

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
        term = get(term.next);
    }
    term_to_string(term, *builder);

    return builder_to_string(*builder);
}

#scope_file

// The coefficient heap stores all coefficients, terms and factors for all nodes in the AST prior to evaluation.
// During evaluation data from this heap is copied into the working set for evaluation purposes which requires mutating terms and factors.
// Once evaluation is complete the heap is reset to be available to work on the next expression.
default_coefficient_heap: Bump_Heap;

// Provide canonical representations for the coefficients zero (0) and one (1).
__zero :: *Coefficient;
__one  :: *Coefficient;
