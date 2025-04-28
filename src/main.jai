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

    // Variables for main loop:
    quit: bool    = false;
    USER_PROMPT :: "> ";
    success: bool = false;
    input: string;

    lexer:  Lexer;
    tokens: [] Token;

    parser: Parser;
    ast:    Ast;

    working_set: Working_Set;

    // Configurations:
    default_dimension: int = 4;
    default_1_elements: [] string = .["e0","e1","e2","e3"];
    default_scalar_variable_prefix_characters: string = "abcdnpqrsuvABCKLMNPQRS";

    update_exterior_space(default_dimension, default_1_elements, default_scalar_variable_prefix_characters);
    initialize_operator_tables();
    initialize_coefficient_heap();
    initialize_working_set(*working_set);

	while !quit {
        
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

        resolve_symbols(*ast, *space);
        if ast_contains_error(ast) {
            ast_print_error(ast);
            continue;
        }

        ast = expand(ast);
        descend(ast);

        ast = flatten(ast);
        descend(ast);

        check_semantics(*ast);
        if ast_contains_error(ast) {
            ast_print_error(ast);
            continue;
        }

        ast = simplify(ast, *working_set); // well it didn't crash so that is good!


/*
        if ast_represents_assignment(ast) {
            // do nothing for now 
        }
        else {
            ast = simplify(ast, *working_set);
        }
*/


/*
    04.08.2025 -- There is no more scratch space. This is called working_set, need to handle intialization and stuff.
                  Also thinking of a separate bump allocator for the coefficient/term/numerators/denominators...

        if ast_represents_assignment(ast) {
         //   simplify_and_assign(ast, *space.user_variables, *scratch);
        }
        else {
          //  ast = simplify(ast, *scratch);
          //  result := ast_to_string(ast);
          //  print("%\n", result);
        }
*/
        report_temporary_storage_metrics();
        report_coefficient_heap_metrics();
        reset_temporary_storage();
        reset_coefficient_heap();
        reset_working_set(*working_set);

    }
}

#scope_file
#import "Basic";
#import "Hash_Table";

report_temporary_storage_metrics :: () {

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
        print("\n");
    }
}