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
    // The total byte size includes the size of the coefficient struct, n-terms and m-numerators and k-denominators.
    total_byte_size: u32;
    term_count:      u16;
    first_term:      Relative_Pointer(Term);
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
    coefficient:  Coefficient;
}

Relative_Pointer :: struct (T: Type) {
    // Offset is declared as an unsigned integer so that all references are located higher in memory!
    offset: u16;
}

assign :: inline (ptr: *Relative_Pointer($T), next: *T) {
    // *Assumes* forward references! Relative pointer calculation will be incorrect if it points into a lower address than it's current location.
    ptr.offset = 0;
    if next { ptr.offset = cast(u16)(cast(u64)next - cast(u64)ptr); }
}

next :: inline (ptr: Relative_Pointer($T)) -> *T {
    return cast(*T)(cast(*u8)*ptr.offset + ptr.offset);
}

starting_term :: (coefficient: *Coefficient) -> *Term {
    return next(coefficient.first_term);
}

next_term :: (term: *Term) -> *Term {
    return next(term.next);
}

term_size :: (term: *Term) -> u32 {
    return cast(u32)(size_of(Term) + (term.numerator_count * size_of(Numerator)) + (term.denominator_count * size_of(Denominator)));
}

coefficient_buffer:  [4097] u8;
coefficient_allocator: [] u8 = coefficient_buffer;

// We want to use a custom allocator that we can pass through here.
// So it needs to adhere to the type
// Then when we get to the thing we need to pass it through, so something like
// The following:

/*
    The 'Allocator' type is simply a package that associates a function to
    a set of data (this is the heap).

    So you can use it on it's own or you can make it an 'Allocator'.
    
    pool: Flat_Pool;
    a: Allocator;
    a.proc = flat_pool_allocator_proc;
    a.data = *pool;
    thing := New(Thing,, a); // then you supply the allocator

    or you can interact with it directly.
    that is the kind of confusing part about it but I guess it makes sense in a certain respect.

    New()
    alloc() --> takes the context.allocator() and uses the allocator data, so this is a pointer to your flat pool.

    so in our case we need to use this allocator for everything related to a coefficient.
    New(Coefficient , Allocator)
    New(Term, Allocator)

    You would have to supply that everywhere and then when you did the working set you need to flip
    Flop around, so you would have an allocator for each and then the 'swap_buffers()' is really to swap the allocators

    allocators: [2] Allocator;
    allocators[0].proc -> Flat_Pool_Allocator_Proc()
    allocators[0].data -> Flat_Pool

    You don't want to supply this to the context because temp allocations could go in the wrong place.
    Could the working set just manage the allocators?

    coefficient_pool_0: 

    allocators[0].proc = 
    allocators[0].data = *coefficient_pool_0
    allocators[1].data = *coefficient_pool_1;

    Fixed_Block_Allocator
    M
    

*/


main :: () {
    
    {
        
    }

    // coefficient_pool; stores coefficient and related data.

    pool: Flat_Pool;
    init(*pool, 4096); // use the default alignment of 8 since it matches our Coefficient, Term and Factor size.

    coefficient := cast(*Coefficient)get(*pool, size_of(Coefficient));

    term := cast(*Term)get(*pool, size_of(Term));
    term.multiples = 10;

    term2 := cast(*Term)get(*pool, size_of(Term));
    term2.multiples = 20;

    x := cast(*u8)get(*pool, 4096);

    assign(*coefficient.first_term, term);
    assign(*term.next, term2);

    first  := starting_term(coefficient);
    second := next_term(first);

    coefficient.term_count = 2;
    coefficient.total_byte_size = size_of(Coefficient) + term_size(term) + term_size(term2);

    print("Starting term value == %\n", first.value);
    print("Second term value == %\n",   second.value);

    print("Pool difference == %\n", pool.current_point - pool.memory_base);


    // These are the actual test cases that need moved into an automated build cycle.
    {
        /*
            v = dir(v) + pos(v)
            v = dir(v) | pos(v)
            v|w & w|v
            (p | dir(P)!) & P   ; this is the projection formula homie!
            -dir(v)!
            -(dir(v)!)
            dir((3+4)*(5+6))
            pos((3 + 4), e1|e2) + basis(3) // NOPE
            5*4^3
            T = v!!
            -!v! should parse as (-(!v))!
            L = dir(p|v!, 4)          
            p = 4*e1;
            pos(arg1);
            pos(arg1,arg2);
            pos(arg1,arg2,arg3) ;
            dir(arg1,(arg2+arg3));
            4*e1|e2 + e1|5*e3
            (-a + b) * (c + d)
            3 * 4 + 5 * 7
            3 + 4 + 5
            3^4 + 5
        */
    }

    // This is me testing stuff out!
    {
        encode_scalar :: (s: string) -> u16 {

            // Format of a scalar symbol is an alphabet character followed by up to 3 digits
            // The smallest number is a000 and the largest is Z999.
            // We map a-z to the numbers 1-26 and A-Z as 26-53.
            // As long as we keep this range we can fit all the scalar symbols hashes into u16s.
            //
            // The hash value zero (0) is reserved for the symbol "1" which is used to represent pure numbers.
            //
            // To build up the number we start at end the end of the string and parse digits as powers of 10
            // then convert the character and add its value.

            assert(s[0] == #char "1" || (s[0] >= #char "a" && s[0] <= #char "z") || (s[0] >= #char "A" && s[0] <= #char "Z"));
            assert(s.count <= 4);

            if s[0] == #char "1" {
                return 1;
            }

            i := s.count-1;

            value:  u16;
            powers: u16 = 1;
            digit:  u16;
            while i > 0 {
                digit  = s[i] - #char "0";
                digit  *= powers;
                value  += digit;

                powers *= 10;
                
                i-= 1;
            }

            if s[0] >= #char "a" && s[0] <= #char "z" {
                digit  = s[i] - #char "a" + 1;
                digit *= powers;
                value += digit;
            }
            else {
                digit  = (s[i] - #char "A") + 27;
                digit *= powers;
                value += digit; 
            }

            assert( value <= 65535 );
            return value;
        }

        s1: string = "1";
        s2: string = "a1";
        s3: string = "b2";
        s4: string = "c1";
        s5: string = "C1";
        s6: string = "a00";
        s7: string = "a01";
        s9: string = "a11";
        s10: string = "a12";
        s11: string = "Z999";

        print("Encoded s1 == %\n", encode_scalar(s1));
        print("Encoded s2 == %\n", encode_scalar(s2));
        print("Encoded s3 == %\n", encode_scalar(s3));
        print("Encoded s4 == %\n", encode_scalar(s4));
        print("Encoded s5 == %\n", encode_scalar(s5));
        print("Encoded s6 == %\n", encode_scalar(s6));
        print("Encoded s7 == %\n", encode_scalar(s7));
        print("Encoded s9 == %\n", encode_scalar(s9));
        print("Encoded s10 == %\n", encode_scalar(s10));
        print("Encoded s11 == %\n", encode_scalar(s11));

        size: int = 64;
        table2: Table(u16, string);
        init(*table2, size);
        
        table_add(*table2, encode_scalar(s1), s1);
        table_add(*table2, encode_scalar(s2), s2);
        table_add(*table2, encode_scalar(s3), s3);
        table_add(*table2, encode_scalar(s4), s4);
        table_add(*table2, encode_scalar(s5), s5);
        table_add(*table2, encode_scalar(s6), s6);
        table_add(*table2, encode_scalar(s7), s7);
        table_add(*table2, encode_scalar(s9), s9);
        table_add(*table2, encode_scalar(s10), s10);

        result, success := table_find(*table2, 111);
        print("Result == %\n", result );

    }

}

#scope_file
#import "Basic";
#import "Hash_Table";
#import "Flat_Pool";
