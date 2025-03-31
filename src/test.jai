#scope_export

term_zero :: Term.{ value = 0 };
term_one  :: Term.{ value = 1 };

coefficient_zero: Coefficient;
coefficient_one:  Coefficient;

Factor :: struct {
    exponent: s16;
    scalar_variable: u16;
}

Term :: struct {
    value: float;
    #place value; multiples: float;

    numerators:  [] Factor;
    denominator: [] Factor;
}

Coefficient :: struct {
    terms: [] Term;
}

main :: () {

    // Just write this down:
    //  "Lines remain parallel and evenly spaced"
    //  Parallel lines are every where equidistant so if lines remain parallel then distances from one parallel line to another doesn't change.
    //  Evenly spaced is across a set of parallel lines
    //     -------------
    //     -------------
    //
    //     ------------- these remain parallel but the distance between them is distored since B and C are twice as far apart

    {
        // Generating results *can* create a lot of temporary overhead.
        // A fixed-sized 'scratch' space is allocated on application start-up. The limits are chosen based on expected usage, primarily
        // with a Grassmann 4D space where each element in an expression is set with either the scalar 1 like '1*e1' or a single
        // scalar variable like 'a1*e1'. With the current limits expressions such as (a1+a2+a3+a4+a5+a6+a7+a8)e1 and
        // (a1a2a3a4..a16 + b1b2b3b4...b16 + c1c2c3c4...c16 + ..)e1 are possible.
        //
        // Depending on the expression, this space can sit 99% unused. It is the price of doing business. The application asserts
        // that no more than 64Kib is being allocated to the space so we know up-front the upper limit of space reserved.
        //
        // Tweaking the max number of numerator or denominator factors only affects their total usage.
        // Increasing the allowable max dimension of the space or the max number of terms allowed per coefficient *can* substantially
        // grow the required scratch space. If it needs bumped up, then so be it, but the current configuration covers my intended
        // use cases.

        MAX_NUMBER_OF_BASIS_ELEMENTS  :: 32;

        COEFFICIENT_PER_BASIS_ELEMENT :: 1;
        MAX_TERMS_PER_COEFFICIENT     :: 8;
        MAX_NUMERATORS_PER_TERM       :: 16;
        MAX_DENOMINATORS_PER_TERM     :: 16;

        TERM_SCRATCH_SPACE                    :: (                            MAX_TERMS_PER_COEFFICIENT * COEFFICIENT_PER_BASIS_ELEMENT * MAX_NUMBER_OF_BASIS_ELEMENTS) * size_of(Term);
        NUMERATOR_SCRATCH_SPACE               :: (MAX_NUMERATORS_PER_TERM   * MAX_TERMS_PER_COEFFICIENT * COEFFICIENT_PER_BASIS_ELEMENT * MAX_NUMBER_OF_BASIS_ELEMENTS) * size_of(Factor);
        DENOMINATOR_SCRATCH_SPACE             :: (MAX_DENOMINATORS_PER_TERM * MAX_TERMS_PER_COEFFICIENT * COEFFICIENT_PER_BASIS_ELEMENT * MAX_NUMBER_OF_BASIS_ELEMENTS) * size_of(Factor);
        WORKING_SET_TERM_SCRATCH_SPACE        :: (                            MAX_TERMS_PER_COEFFICIENT * COEFFICIENT_PER_BASIS_ELEMENT                               ) * size_of(Term);
        WORKING_SET_NUMERATOR_SCRATCH_SPACE   :: (MAX_NUMERATORS_PER_TERM   * MAX_TERMS_PER_COEFFICIENT * COEFFICIENT_PER_BASIS_ELEMENT                               ) * size_of(Factor);
        WORKING_SET_DENOMINATOR_SCRATCH_SPACE :: (MAX_DENOMINATORS_PER_TERM * MAX_TERMS_PER_COEFFICIENT * COEFFICIENT_PER_BASIS_ELEMENT                               ) * size_of(Factor);

        TOTAL_SCRATCH_SPACE :: 
              TERM_SCRATCH_SPACE 
            + NUMERATOR_SCRATCH_SPACE 
            + DENOMINATOR_SCRATCH_SPACE 
            + WORKING_SET_TERM_SCRATCH_SPACE
            + WORKING_SET_NUMERATOR_SCRATCH_SPACE
            + WORKING_SET_DENOMINATOR_SCRATCH_SPACE;

        #assert(TOTAL_SCRATCH_SPACE < 65536 );

        print("TERM_SCRATCH_SPACE                    == %\n", TERM_SCRATCH_SPACE);
        print("NUMERATOR_SCRATCH_SPACE               == %\n", NUMERATOR_SCRATCH_SPACE);
        print("DENOMINATOR_SCRATCH_SPACE             == %\n", DENOMINATOR_SCRATCH_SPACE);
        print("WORKING_SET TERM SCRATCH SPACE        == %\n", WORKING_SET_TERM_SCRATCH_SPACE);
        print("WORKING_SET NUMERATOR SCRATCH SPACE   == %\n", WORKING_SET_NUMERATOR_SCRATCH_SPACE);
        print("WORKING_SET DENOMINATOR SCRATCH SPACE == %\n", WORKING_SET_DENOMINATOR_SCRATCH_SPACE);
    }
    
    // TODO: Use this in a separate file called 'Scratch.jai' 

    MAX_NUMERATORS_PER_TERM      :: 100;
    MAX_DENOMINATORS_PER_TERM    :: 100;

    MAX_TERMS_PER_BASIS_ELEMENT  :: 10;

    COEFFICIENT_SCRATCH_SPACE_PER_BASIS_ELEMENT :: MAX_TERMS_PER_BASIS_ELEMENT * size_of(Term);
    MAX_NUMBER_OF_BASIS_ELEMENTS :: 16;

    TOTAL_TERM_SCRATCH_SPACE        :: MAX_NUMBER_OF_BASIS_ELEMENTS * (MAX_TERMS_PER_BASIS_ELEMENT * size_of(Term));
    TOTAL_NUMERATOR_SCRATCH_SPACE   :: MAX_NUMBER_OF_BASIS_ELEMENTS * (MAX_TERMS_PER_BASIS_ELEMENT * MAX_NUMERATORS_PER_TERM) * size_of(Factor);
    TOTAL_DENOMINATOR_SCRATCH_SPACE :: MAX_NUMBER_OF_BASIS_ELEMENTS * (MAX_TERMS_PER_BASIS_ELEMENT * MAX_NUMERATORS_PER_TERM) * size_of(Factor);

    print("Total term scratch space == %\n", TOTAL_TERM_SCRATCH_SPACE);
    print("Total numerator scratch space == %\n", TOTAL_NUMERATOR_SCRATCH_SPACE);
    print("Total denominator scratch space == %\n", TOTAL_DENOMINATOR_SCRATCH_SPACE);


    // High water mark to make sure I don't change any of the settings and start allocating more memory than I intend.
    // 64kb on a modern system is literally nothing but just a sanity check since we are under 4kb of scratch space at the moment.
    #assert(TOTAL_TERM_SCRATCH_SPACE < 65536); 

    Scratch :: struct {
        numerators:    [TOTAL_NUMERATOR_SCRATCH_SPACE]   u8;
        denominators:  [TOTAL_DENOMINATOR_SCRATCH_SPACE] u8;
        terms:         [TOTAL_TERM_SCRATCH_SPACE] u8;
        coefficients:  [MAX_NUMBER_OF_BASIS_ELEMENTS] Coefficient; // Each index represents the current result for that basis element during evaluation
    }

    scratch: Scratch;
    init_scratch_space :: (scratch: *Scratch) { // TODO: call this on program startup!
        // Space is statically allocated, just adjust the pointers for the coefficients to their respective sections of the scratch buffer
        for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
            scratch.coefficients[i].terms.data = cast(*Term)*scratch.terms[i * COEFFICIENT_SCRATCH_SPACE_PER_BASIS_ELEMENT];
            scratch.coefficients[i].terms.count = 0;
        }
    }

    reset_scratch_space :: (scratch: *Scratch) {
        // Do *not* adjust the pointers into the scratch space buffer, just reset counts back to zero.
        for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
            scratch.coefficients[i].terms.count = 0;
        }
    }

    init_scratch_space(*scratch);
    for i: 0..MAX_NUMBER_OF_BASIS_ELEMENTS-1 {
        diff := cast(u64)scratch.coefficients[i].terms.data - cast(u64)scratch.terms.data;
        print("Diff == %\n", diff);
    }

    reset_scratch_space(*scratch);

  
    init_coefficients :: () { // TODO: call this on startup!
        coefficient_zero.terms.data  = *term_zero;
        coefficient_zero.terms.count = 1;
        
        coefficient_one.terms.data = *term_one;
        coefficient_one.terms.count = 1;

        return;
    }

    zero :: () -> Coefficient {
        return coefficient_zero;
    }

    one :: () -> Coefficient {
        return coefficient_one;
    } 

    // THIS ENDS SCRATCH SPACE!

    /*
        We create a new number:
          Basis element with a new coefficient
          When we resolve the identifier then we can point to this so e0 + e1 + e2 + e3 no longer allocates 64 * 4
          Coefficient is a List(Term) --> allocate space for a term, create the term and store it in that location. (this is in temporary storage)

          The scratch space should just store 0 and 1, that is the space for them!

        

    */

    


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