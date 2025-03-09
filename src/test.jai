#scope_export

main :: () {

    {
        Node_Kind :: enum u8 {
            Symbol;
            Basis_Element;
            Scalar;        
            Scalar_Expression;
            Unary_Operator;     // prefix operators
            Binary_Operator;    // infix/postfix operators
            Intrinsic;
        }

        Ast_Node :: struct {
            kind: Node_Kind;
        }

        Scalar :: struct {
            value:       float;
            exponent:    s16;
            symbol_hash: u16;
        }

        Term :: struct {
            multiples:   int;
            num_factors: u16;
            next_factor: u16;
            factors: [6] Scalar; // TODO: this should be fixed based on the basis of the space
        }

        print("Size of term == %\n", size_of(Term));

        Scalar_Expression_Node :: struct {
            #as using _kind: Ast_Node;
            terms: List(Term);
        }

        // with this we generate the minimal number of whatever
        // i kind of what this to return a list, not a pointer to list so we can embed it
        // then terms is a List(Term)

        // List(Term) which means it has head, tail and whatever else in there
        // just points directly to the list instead of being indirect.
        // don't even have to initialize it, just use it and it will figure out what to do, since these are all zeroes

        // say we add 3 + a1 this generates two terms
        factor1: Scalar = .{ value = 3, exponent = 1, symbol_hash = 1};
        term1:   Term;
        term1.factors[term1.next_factor] = factor1;
        term1.num_factors += 1;
        term1.next_factor += 1;

        factor2: Scalar = .{ value = 1, exponent = 1, symbol_hash = 11};
        term2:   Term;
        term2.factors[term2.next_factor] = factor2;
        term2.num_factors += 1;
        term2.next_factor += 1;

        node: Scalar_Expression_Node;
        list_append(*node.terms, term1);
        list_append(*node.terms, term2);

        c := list_head(node.terms);
        i := 0;
        while c != null {
            term := c.value;
            print("Printing term %\n", i);
            for i: 0..term.num_factors-1 {
                print("Factor == %\n", term.factors[i]);
            }
            c = c.next;
            i += 1;
        }
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
            L = dir(p|v!, 4)          // this doesn't look right!
            p = 4*e1; // WORKED
            pos(arg1); // WORKED!
            pos(arg1,arg2);
            pos(arg1,arg2,arg3) ; 02.23.2025 this no workie either!
            dir(arg1,(arg2+arg3));  02.23.2025 This doesn't work anymore; wonder if that was what the other logic was about? with the open paren in the infix section?
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
#load "list.jai";