#scope_export

main :: () {

    {
        numbers: [..] int;  // An empty array that can automatically expand.

        for 1..5 array_add(*numbers, it*it);

        for < number: numbers  print("'number'  backwards: %\n", number);
    }

    for  #v2 < 1..5 print ("hello");

    {
        prefix_characters: string = "abcdnpqrsuvABCKLMNPQRS";
        symbol := "p";
        for c: prefix_characters {
            print("Did we find it?\n" );
            if symbol[0] == c {
                break;
            }
        }
/*

        x := encode("a1");
        y := encode("a01");
        z := encode("b1");
        w := encode("B1");

        t1 := encode("z99");
        t2 := encode("Z99");

        print("Encoded x=%, y=%, z=%, w=%, t1=%, t2=%\n", x,y,z,w,t1,t2);
        print("Decoded x=%, y=%, z=%, w=%, t1=%, t2=%\n", decode(x), decode(y), decode(z), decode(w), decode(t1), decode(t2));
    
*/
        // 4 bytes (32-bit)
        // char numindices 0 1
        // char 1 1
        // they would be represented as the same thing
        // char 1 1
        // char 2 0 1
        // char 2 1 1
        //

        // we want ordered insert of factors into a term so we can memcmp them, if you store the strings then that isn't efficient
        // if you store a pointer then the order isn't defined
        // 
        // preferably would like to store
        //  char 'a'
        //  index: 01
        //
        //  index: 1

        // want to easy encode it and decode it, nothing
        // it has to be unique so that scalar symbols don't 
        // then we don't have to store anything about them, we simply search the prefix_characters list then deal with i


        // go from left to right, get the length of the identifier
        // a1  --> 11
        // a01 --> 101
        // 
        // encode a11 -- start at the end and work backwards 

        // encode a 101
        //        b 2
        //        c 3

        // okay so the question is how do we encode this to match our scheme?
        // because we returned an index into a list but now we might not have to do that?
        //
        // What if we encoded as 4 bytes
        // it would be cool to encode and be able to undo the encoding
    }

    {
        // so now on symbol resolution you would just say
        // you could still hash them but it isn't necessary. I really don't know how to handle this quite yet
        // you'd iterate through and check, you'd know that these are
        // i'd like to be able to do a11 a21 a22 to do it like a matrix, and compare formulas that way

        // so you check and say yes 'it matches the scalar_variables.prefix_characters[i]
        // then if you allow a01, a00 a10, a01 is one, and you make that two digits, two bytes 'char',0,1

        x: u8 = #char "0";
        builder: String_Builder;
        append(*builder, #char "a");
        append(*builder, x); // could store up to 256
        append(*builder, #char "1");

        s := builder_to_string(*builder);
        print("s == %\n", s);
    }
   
   {
        Node_Kind :: enum u8 {  

            Identifier;
            Variable;
            Element;            // element of an exterior space
            Unary_Operator;     // prefix operators
            Binary_Operator;    // infix/postfix operators
            Intrinsic;
        }

        Ast_Node :: struct {
            kind: Node_Kind;
        }

        Ast :: struct {
            root: *Ast_Node;
        }

        // So we have two options here, we could just allocate an Element node OR we could try to patch this bad boy up to be a basis element
        // this would be cool if we could do it.

        // *Variable_Node
        // *Element_Node

        Identifier_Node :: struct {
            #as using _kind: Ast_Node; // Identifier --> Variable (could do this for variables) then allocate a new Element for a scalar or basis element.
            union {
                name: string;
                value: int;
            }
        }

        ast: Ast;
        node: *Identifier_Node = talloc(size_of(Identifier_Node));
        node.kind = .Identifier;
        node.name = "world";

        ast.root = node;

        if ast.root.kind == .Identifier {
            ident := cast(*Identifier_Node)ast.root;
            ident.kind = .Variable;
            ident.name = "StrongBad";
        }

        // our variables will be small, so i don't know man doesn't seem like too big of a deal
        
        if ast.root.kind == .Variable {
            var := cast(*Identifier_Node)ast.root;
            print("var.name == %\n", var.name);
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