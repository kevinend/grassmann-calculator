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
    // Context-dependent value based on the currently evaluated expression.
    terms:      *Term;
    term_count:  int;
    byte_size:   int;
}

Basis_Element :: struct {
    // Immutable defintion of a basis element from grade [0,n] in the currently declared space.
    grade:  int;
    bitset: int;
    symbol: string;
}

Basis_Term :: struct {
    // Represents a single basis element term in an expression such as (a1e1 + a2e2) or (a1a2)e1^e2.
    basis:       *Basis_Element;
    coefficient: *Coefficient;
}

initialize_canonical_basis_terms :: () {

    basis_term_zero.basis = get_basis_element(0);
    basis_term_zero.coefficient = new_coefficient(0);

    basis_term_one.basis = get_basis_element(0);
    basis_term_one.coefficient = new_coefficient(1);
}

zero :: () -> Basis_Term { return basis_term_zero; }
one  :: () -> Basis_Term { return basis_term_one;  }

coefficient_zero :: () -> *Coefficient { return zero().coefficient; }
coefficient_one  :: () -> *Coefficient { return one().coefficient;  }

new_coefficient :: (value: float) -> *Coefficient {

    coefficient := cast(*Coefficient)alloc(size_of(Coefficient));
    assert(coefficient != null);

    coefficient.terms = new_term(value);
    coefficient.term_count = 1;
    coefficient.byte_size  = size_of(Term);

    return coefficient;
}

new_coefficient :: (multiples: float, exponent: s16, scalar_variable: string) -> *Coefficient {

    coefficient := cast(*Coefficient)alloc(size_of(Coefficient));
    assert(coefficient != null);

    coefficient.terms = new_term(multiples, exponent, scalar_variable);
    coefficient.term_count = 1;
    coefficient.byte_size  = size_of(Term) + size_of(Numerator);

    return coefficient;
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
#import "Basic";

// Canonical representations of the values 0 and 1.
basis_term_zero: Basis_Term;
basis_term_one:  Basis_Term;

// TODO: 04.13.2025, this is all speculative until reviewed below!

new_term :: (value: float) -> *Term {

    term := cast(*Term)alloc(1*size_of(Term));
    assert(term != null);

    term.value = value;
    term.numerator_count = 0;
    term.denominator_count = 0;
    assign(*term.next, null);

    return term;
}

// TODO: 04.13.2025 - Want to use a specific buffer just for the coefficient's rather than temp.

new_term :: (multiples: float, exponent: s16, scalar_variable: string) -> *Term {

    memory: *u8 = alloc(size_of(Term) + size_of(Numerator));
    assert(memory != null);

    term := cast(*Term)memory;
    term.multiples = multiples;
    term.numerator_count = 1;
    term.denominator_count = 0;
    assign(*term.next, null);

    numerator := cast(*Numerator)(memory + size_of(Term));
    numerator.exponent = exponent;
    numerator.scalar_variable = encode(scalar_variable);

    return term;
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