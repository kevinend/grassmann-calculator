#scope_export

main :: () {
    {
        Factor :: struct {
            exponent: 			   u32;
            scalar_variable_index: u32;
        }

        Term :: struct {
            // Represents a single number or a list of scalar-variable factors to represent terms such as "a1" or a product like "a1b2c3"
            // This is like a tagged union without the tag. A number is implicit and occurs when the count of factors is zero.

            value: float;
            #place value; multiples: float;

            num_factors: u32;
            factors:     [] Factor;
            next: *Term;
        }

        print("Size of term KEVIN == %\n", size_of(Term));
    }

    // The Coefficient is a list of terms
    // The Terms is a list of factors...

    // if we store pointers then we can't memcmp these but if we store offsets then we could

    {
        // could overload operators but don't like that
        
        // 03.09.2025
        Factor :: struct {
            exponent:      u32;
            variable_hash: u32;
        }

        Term :: struct {
            // Represents either a pure number such as 1, 2 or 3 or contains a list of scalar-variable factors to represent terms
            // such as "a1" or a product like "a1b2c3"
            value: float;
            #place value; multiples: float;

            factors: [] Factor;
            next: *Term;
        }

        Basis :: struct {
            nothing: int;
        }

        Element :: struct {
            coefficient: *Term;
            basis:       *Basis;
        }

        operator == :: (lterm: Term, rterm: Term) -> bool {
            // Terms are equal when the following conditions are true.
            //  1. Each term contains the same number of factors (including zero).
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

        /*
        combine_like_terms
          Input  = (3 + a1) + (b1 + 4 + a1)
          Output = (7 + 2a1 + b1)
          Copy the lhs terms into a new buffer.
          Iterate over each term in the copy (iterate by index, not .next or when you add to this list you will keep iterating unnecessarily)
           Iterate over each term in the rhs
           if terms_are_equal(lterm, rterm) {
                // This is the body for the 'terms are equal piece'
                if lterm.factor.count == rterm.factor.count {
                    count := lterm.factor.count;
                    return memcmp(lterm.factors.data, rterm.factors.data, size_of(Factor) * count);
                }
                else {
                    return false;
                }
                
                copy.multiples = lterm.multiples + rterm.multiples;
                // keep track of the previous node then link the previous to the next so that factor is excluded
                // in future comparisons.
           }
           else {
                couldn't combine terms add the new term!
                TODO: how do you do this without iterating over the new term?
           }
        */

        t1: Term;
        t2: Term;

        t1.value = 10;
        t2.value = 20;

        print("Memcmp for pure numbers returns %\n", t1 == t2 );

        t1.factors.data = alloc(1);
        t1.factors.count = 1;
        t2.factors.data = alloc(2);
        t2.factors.count = 1;

        t1.factors[0] = .{ exponent = 1, variable_hash = 10 };
        t2.factors[0] = .{ exponent = 1, variable_hash = 11 };

        print("Memcmp for factors returns %\n", t1 == t2);

        // then for basis elements we use grade 0 and do what we did in the past
        // can traverse the list of factors then do a memcmp from the exponent

        print("Size of factor == %\n", size_of(Factor));
        print("Size of term == %\n",   size_of(Term));

    }

    {
        MAX_DIMENSION :: 4;
        Product :: struct {
            num_factors: int;
            factors:     [MAX_DIMENSION] Scalar_Variable;

        }

        Scalar_Variable :: struct {
            multiples:        s32;
            exponent:         s16;
            identifier_hash:  u16;
        }

        Term_Type :: enum {
            Value;
            Variable;
            Product;
        }

        Term :: struct {
            type: Term_Type;
            union {
                value:    int;
                variable: Scalar_Variable;
                product:  *Product;
            }
            next_term: *Term;
        }

        Basis :: struct {
            coefficient: Term;
        }

        /*
            if lhs.grade == rhs.grade
                result.coefficient = add_coefficients(lhs.coefficient, rhs.coefficient);
                result.basis = lhs.basis;
            
                
            add_coefficients(Lterm: Term, Rterm: Term) -> term {
                result: Term;
                if Lterm.type == .Value && Rterm.type == .Value {
                    result.type  = .Value;
                    result.value = Lterm.value + Rterm.value;
                }
                else
                if (Lterm.type == .Variable && Rterm.type == .Variable) && (Lterm.variable.symbol == Rterm.variable.symbol) {
                    // a1 + a1
                }
                else {
                    // Iterate over the list of terms
                }
            }

        */

        // how would you do this?
        // you Addition -> BasisElement1 and Basis_Element2
        // 
        //                
        // 
        // 
        //        if Lterm.type == .Value && Rterm.type == .Value 
        // add_coefficients( lhs.coefficient, rhs.coefficient );
        // result: Term;
        // 

        // so before we go hog wild let's double check the math here.
        // 3 + 4
        // term.type = .Value, 3  term.type = .Value, 4 (now we don't have to hash 1 and have that confusing piece)
        //   lhs.coefficent.value += rhs.coefficient.value;
        // 
        // a1 + a1
        // term.type = .Variable, a1 term.type == .Variable
        //   add them together
        //   term.type = .Variable, term.variable.multiples = add them together. exponent = lhs.exponent, identifier_hash = lhs.identifier_hash;
        //
        // 3 + a1
        // coefficient.type == .Value && coefficient.type == .Variable
        // this generates a new list of terms with each value
        // do we just link them together?
        // result just joins the two links together
        // basis.coefficient = lhs.coefficient;
        // basis.coefficient.next = rhs.coefficent;
            
        print("Size of term == %\n", size_of(Term));
    }

    // These are the actual test cases.
    // TODO: Figure out how to test these out! and run these automated on a build!
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