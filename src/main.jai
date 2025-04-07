#scope_export

/*
    High-Level steps of the application
        // Lexer             --> Generate tokens.
        // Parser            --> AST with operators, keywords and identifiers.
        // Symbol Resolution --> Pass over the tree and replace all identifiers with their stored AST values.
        // Expand            --> Distribute products over addition and subtraction.
        // Semantic Pass     --> Pass over the tree again and make sure that scalar symbols aren't being assigned values, that scalars are only used in scalar multiplication, etc.
        // Simplify          --> Evaluate the expression, generate the K-Element represented by the user, store if an assignment
        // Display           --> Convert the k-element to a string and display that to the end user.

    Scalar Symbols:
        Rules: Allow a single character, could also add these to a list.
        p,q,r,s   - points
        a,b,c,u,v - vectors
        A,B,C     - bivectors  (4 there are 6 of these basis) A1*e1|e2 + A2*e2|e3, A B C D E F E = E1e
        K,L,M,N   - line
        P,Q,R,S   - plane
*/

main :: () {

    // Configurations:
    default_dimension: int = 4;
    default_1_elements: [] string = .["e0","e1","e2","e3"];
    default_scalar_variable_prefix_characters: string = "abcdnpqrsuvABCKLMNPQRS";

    scratch: Scratch;

    update_exterior_space(default_dimension, default_1_elements, default_scalar_variable_prefix_characters);
    initialize_operator_tables();
    initialize_coefficients();
    initialize_scratch_space(*scratch);
   
    // Variables for main loop:
    quit: bool    = false;
    USER_PROMPT :: "> ";
    success: bool = false;
    input: string;
    
    lexer:  Lexer;
    tokens: [] Token;

    parser: Parser;
    ast:    Ast;

	while !quit {
        ts := context.temporary_storage;
        if !ts {
            print("No temporary storage!\n");
        }
        else {
            print("Temporary storage metrics\n");
            print("Size == %\n", ts.original_size);
            print("Current_page_bytes_occupied == %\n", ts.current_page_bytes_occupied);
            print("Total bytes occupied == %\n", ts.total_bytes_occupied);
            print("High water mark == %\n", ts.high_water_mark);
        }

/*
        ts.data = ts.original_data;
        ts.size = ts.original_size;
        ts.current_page_bytes_occupied = 0;
        ts.total_bytes_occupied = 0;
        ts.high_water_mark = 0;
*/

        reset_temporary_storage();
        
		print("%", USER_PROMPT);
		success, input = read_line();
        if !success {
            flush_input();
            continue;
        }
        else
        if input.count == 0 {
            print( "No input; Enter a valid Grassmann expression or 'quit' to exit the application.\n" );
            continue;
        }
        else
        if input == "quit" || input == "exit" {
            quit = true;
            continue;
        }

        success, tokens = generate_tokens(*lexer, input);
        if !success {
            error := get_lexer_error(*lexer);
			print(error);
			continue;
		}

        success, ast = parse(*parser, tokens, input);
        if !success {
            error := get_parser_error(*parser);
            print(error);
            continue;
        }

        /*  Transformations:
        --> Resolve Symbols
               ^
              v  e1

        -->    ^        -->    ^       -->       ^                  -->     ^
             +    e1         +    +            +    +                      +    +
            e1 e2         e1  e2 e3 e4     e1  e2  e3   +               e1  e2 e3 ^
                                                     e4   e5                     e4 e5

                                                    
        --> Flatten 
            ^                    ^               ^                          ^
            +  e1            +      +           +       +                 +     +     
            e1 e2           e1, e2  e3,e4       e1 e2  e3 e4 e5         [e1,e2] [e3, ^]
                                                                                    [e4,e5]

                                                                        (e1+e2) ^ (e3 + (e4 ^ e5))
                                                                        (e1+e2)^e3 + (e1+e2)^(e4^e5)
                                                                        
                                                                        (e1^e3)+(e1^e2) + (e1 ^ (e4^e5)) + (e2^(e4^e5))
                                                                        +
                                                                        [^]     [^]      [^]        [^]
                                                                        [e1,e3] [e1,e2]  [e1, ^]    [e2, ^]
                                                                                             [e4,e5]    [e4,e5]

                                                                        then you just build that back up


        --> Distribute
            +
            ^       ^       ^       ^         ^        ^
            [e1,e3] [e1,e4] [e1,e5] [e2,e3], [e2,e4], [e2,e5]           

        // Resolve symbols (replace v ^ e1 with (e1 + e2 + e3) ^ e1)
        // Flatten         (replace (e1 + e2 + e3) ^ e1) with
        //                 ^
        //                 e1 + e1 e2 e3
        // Distribute      +
        //                 ^
        //
        // Semantic Check
        // Evaluate (simplify and simplify and assign)              


/*
        descend(ast.root);

        resolve_symbols(*ast, *space);
        descend(ast.root);

        pass := check_semantics(*ast);
        if !pass {
            continue;
        }

        // Expand and simplify return new instances of the entire syntax tree.
        // Cloning the nodes wastes memory but we are using the linear allocator so new nodes will be located near one another
        // so maybe not a huge problem? 03.18.2025

        ast = expand(ast);
        descend(ast.root);

        if ast_represents_assignment(ast) {
            simplify_and_assign(ast, *space.user_variables, *scratch);
        }
        else {
            ast = simplify(ast, *scratch);
            result := ast_to_string(ast);
            print("%\n", result);
        }
*/
    }
}

#scope_file
#import "Basic";
#import "Hash_Table";