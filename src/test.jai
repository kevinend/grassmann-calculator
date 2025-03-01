#scope_export

Factor :: struct {
    s: string;
}

Term :: struct {
    factors: [..] Factor;
}

Scalar_Node :: struct {
    terms: [..] Term;
}

main :: () {
    
    // how do you want to do this?
    // need a list of factors

    context.allocator = temp;

    factor: Factor;
    factors: [..] Factor;

    term: Term;
    terms: [..] Term;

    array_add(*factors, factor);
    term.factors = factors;

    array_add(*terms, term);

    node: *Scalar_Node = alloc(size_of(Scalar_Node));
    node.terms = terms;
 //   node.terms[0].factors = factors;

    print("Terms.count   == %\n", node.terms.count );
    print("Factors.count == %\n", node.terms[0].factors.count);

    array_add(*node.terms, term);
    array_add(*node.terms[1].factors, factor);

    array_add(*node.terms[1].factors, factor);

    print("Terms.count   == %\n", node.terms.count );
    print("Factors.count == %\n", node.terms[0].factors.count);
    print("Factors.count == %\n", node.terms[1].factors.count);
    

   
    

    


   // array_add(*term.factors, factor);
   // array_add(*node.terms,   term  );

//    print("Term.factors.count == %\n", term.factors.count);
 //   print("Node.terms.count   == %\n", node.terms.count );

    da: [..] int;
    array_add(*da, 4);
    print("da.count == %\n", da.count);
    for x: da {
        print( "x == %\n", x );
    }

    

    initial_table_size: int = 64;
    table: Table(string, string);
    init(*table, initial_table_size);

    s1: string;
    s1.data = alloc(5);
    s1.count = 5;
    s1[0] = #char "H";
    s1[1] = #char "e";
    s1[2] = #char "l";
    s1[3] = #char "l";
    s1[4] = #char "o";

    s2: string = "World";
    s3: string = "Hello";

    table_add(*table, s1, s1);
    table_add(*table, s2, s2);

    s11: string;
    s13: string;
    success: bool;
    s11, success = table_find(*table, s1);
    s13, success = table_find(*table, s3);
    print("s11 == %\n", s11);
    print("s13 == %\n", s13);
    print("s1.data == s3.data; %\n",   s1.data == s3.data );
    print("s11.data == s13.data; %\n", s11.data == s13.data);


}

#scope_file
#import "Basic";
#import "Hash_Table";