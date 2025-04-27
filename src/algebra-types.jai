/*
        Approach 1: Relative pointers and a single fixed size buffer for everything.

        (1) Pure number
        coefficient := new_coefficient();
        term        := new_term(value);
        append_term(term, coefficient);

        (2) Scalar variable
        coefficient := new_coefficient();
        term        := new_term(multiples);
        numerator   := new_factor(.NUMERATOR, exponent, scalar_variable);
        append_term(term, coefficient);
        append_factor(numerator, term);

        // when we copy then we have to do pointer-fixup

        then when we say things like allocate_term() we implicitly use these
        then there is an overload where we pass in the working_set

        allocate_term(working_set, multiples);
        allocate_term(working_set, value);

        // if the first factor then 

    // we need to pass through the result which is a coefficient
    //
    // the code goes something like:
    //  multiply_coefficients(lhs: )
    //    multiply_terms(lhs:)
    //    add term
    //      product := allocate_term(result.coefficient, lhs.term.multiples * rhs.term.multiples)
    //      multiply_factors(lhs.factors, rhs.factors, product)
    //      append_term(product, result.coefficient)
    //      
    //    since that term knows the heap it can allocate it in the correct buffer...
    // I honestly don't love it but it seems to work okay

    // Loop example:

    current := c0.first_term;
    for i: 0..c0.term_count-1 {
        Basic.print("Value == %\n", current.value);
        current = next_term(current);
    }

       /*
            
            Lterm := lhs.first_term;
            for i: 0..lhs.term_count-1 {
                Rterm := rhs.first_term;
                for j: 0..rhs.term_count-1 {
                    if equal_terms(Lterm, Rterm) {
                        Lterm.value += Rterm.value;
                        remove_term(Rterm, rhs); // this needs to adjust the count
                    }
                    Rterm = next_term(Rterm);
                }
                Lterm = next_term(Lterm);

            }
        */
*/

#scope_export

Factor_Type :: enum u8 {
    Numerator;
    Denominator;
}

Factor :: struct {
    type:            Factor_Type;
    exponent:        u16;
    scalar_variable: u16; // MAKING THIS A u32 leaving for now to get over error - 04.26.2025
}

Term :: struct {
    value: float;
    #place value; multiples: float;

    // Factors include both numerators and denominators.
    // Denominators follow immediatetly after any numerators.
    factors: Relative_Pointer(Factor);
    factor_count: s16; // *must* be signed; for loop when 0 will underflow otherwise

    previous: Relative_Pointer(Term);
    next:     Relative_Pointer(Term);
}

Coefficient :: struct {
    heap:       *Bump_Heap;
    first_term: Relative_Pointer(Term);
    last_term:  Relative_Pointer(Term);
    term_count: s32; // *must* be signed; for loop when 0 will underflow otherwise
}

Basis_Element :: struct {
    // Immutable defintion of a basis element from grade [0,n] in the currently declared space.
    grade:  int;
    bitset: int;
    symbol: string;
}

Basis_Term :: struct {
    // Represents a single basis element term in an expression such as 3e1 or (a1a2)e1^e2.
    basis:       *Basis_Element;
    coefficient: *Coefficient;
}

get_basis_bitset :: (basis_term: *Basis_Term) -> int {
    return basis_term.basis.bitset;
}

new_coefficient :: (value: float) -> *Coefficient {

    coefficient := cast(*Coefficient)heap_allocate(*coefficient_heap, size = size_of(Coefficient), alignment = size_of(Coefficient));
    assert(coefficient != null);
    memset(coefficient, 0, size_of(Coefficient));

    term := new_term(value);
    append_term(term, coefficient);

    return coefficient;
}

new_coefficient :: (multiples: float, factor_type: Factor_Type, exponent: u16, scalar_variable: string) -> *Coefficient {

    coefficient := cast(*Coefficient)heap_allocate(*coefficient_heap, size = size_of(Coefficient), alignment = size_of(Coefficient));
    assert(coefficient != null);
    memset(coefficient, 0, size_of(Coefficient));

    term   := new_term(multiples);
    factor := new_factor(factor_type, exponent, scalar_variable);

    append_term(term, coefficient);
    append_factor(factor, term);

    return coefficient;
}

new_term :: (value: float, heap: *Bump_Heap) -> *Term {

    term := cast(*Term)heap_allocate(*coefficient_heap, size = size_of(Term), alignment = size_of(Term));
    assert(term != null);

    memset(term, 0, size_of(Term));
    term.value = value;
    return term;
}

new_factor :: (type: Factor_Type, exponent: u16, scalar_variable: string) -> *Factor {

    factor := cast(*Factor)heap_allocate(*coefficient_heap, size = size_of(Factor), alignment = size_of(Factor));
    assert(factor != null);

    memset(factor, 0, size_of(Factor));
    factor.type     = type;
    factor.exponent = exponent;
    factor.scalar_variable = encode(scalar_variable);

    return factor;
}

append_term :: (term: *Term, coefficient: *Coefficient) {

    if coefficient.term_count == 0 {
        set(*coefficient.first_term, term);
        set(*coefficient.last_term,  term);
    }
    else {
        last := get(coefficient.last_term);
        set(*last.next, term);
        set(*term.next, null);
        set(*term.previous, last);

        set(*coefficient.last_term, term);
    }

    coefficient.term_count += 1;
    return;
}

append_factor :: (factor: *Factor, term: *Term) {

    // TODO: This needs thought through more on how to handle a mix of numerators and denominators. 04.26.2025
    assert(term != null);
    if term.factor_count == 0 {
        set(*term.factors, factor);
    }
    
    term.factor_count += 1;
    return;
}

get_coefficient_size :: (coefficient: *Coefficient) -> int {
    start := cast(*u8)coefficient;

    last  := get(coefficient.last_term);
    end   := cast(*u8)(get(last.factors) + last.factor_count);

    return end - start;
}

initialize_coefficient_heap :: () {

    // Initialize coefficients that are used constantly for expressions, namely zero and one.
    _zero = new_coefficient(0);
    _one  = new_coefficient(1);

    // Set checkpoint so that when we reset we leave zero and one untouched for subsequent executions.
    heap_set_checkpoint(*coefficient_heap);

    return;
}

zero :: () -> *Coefficient { return _zero; }
one  :: () -> *Coefficient { return _one;  }

reset_coefficient_heap :: () {
    heap_reset_to_checkpoint(*coefficient_heap);
    return;
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

report_coefficient_heap_metrics :: () {
    heap_print(*coefficient_heap);
    return;
}

negate :: (input: Basis_Term, output: *Basis_Term) {
    return;
}

complement :: (left: bool, k: Basis_Term, result: *Basis_Term ) { return; }
sum        :: (lhs: Basis_Term, rhs: Basis_Term, result: *Basis_Term) { return; }


#scope_file

// The coefficient heap stores all coefficients, terms and factors for all nodes in the AST prior to evaluation.
// During evaluation data from this heap is copied into the working set for evaluation purposes which requires mutating terms and factors.
// Once evaluation is complete the heap is reset to be available to work on the next expression.
coefficient_heap: Bump_Heap;

_zero: *Coefficient;
_one:  *Coefficient;

/*
negate :: (input: Basis_Term, output: *Basis_Term) {

    output.basis = input.basis;
    output.coefficient.term_count       = input.coefficient.term_count;
    output.coefficient.total_byte_size  = input.coefficient.total_byte_size;

    memcpy(output.coefficient, input.coefficient, input.coefficient.total_byte_size);

    term := output.coefficient.terms;
    for i: 0..output.coefficient.term_count-1 {
        term.multiples *= -1;
        term = get(term.next);
    }

    return;
}

complement :: (left: bool, k: Basis_Term, result: *Basis_Term ) {

    // Generates an (n-k)-element from the input k-element.
    
    bits        := space.n_element.bitset ^ k.basis.bitset;
    basis       := get_basis_element(bits);
    coefficient := 1;

    return;
}

sum :: (lhs: Basis_Term, rhs: Basis_Term, result: *Basis_Term) {
    
    // Result is pre-allocated space in the working set to store the value.
    
    copy_basis_term(src = lhs, dst = result);

    Lterm := result.coefficient.first_term;
    Rterm := rhs.coefficient.first_term;

    // TODO: 04.17.2025 -- need to use the NEXT function to get the next term!
    for i: 0..result.coefficient.term_count-1 {
        for j: 0..rhs.coefficient.term_count-1 {
            if terms_are_equal(Lterm, Rterm) {
                Lterm.multiples += Rterm.multiples;
            }
        }
    }

    // Append the remaining terms from the RHS that could *not* be combined with the LHS.
    for j: 0..rhs.coefficient.term_count-1 {
        // new term in the allocator
    }

    return;
}

*/