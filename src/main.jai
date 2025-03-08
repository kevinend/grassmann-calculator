#scope_export

/*
    High-Level steps of the application
        // Lexer             --> tokens
        // Parser            --> AST with operators, keywords and symbols
        // Symbol Resolution --> Pass over the tree and replace all identifiers with their stored AST values.
        // Semantic Pass     --> Pass over the tree again and make sure that scalar symbols aren't being assigned values, that scalars are only used in scalar multiplication, etc.
        // Expand            --> Distribute grassmann products over addition and subtraction
        // Simplify          --> Evaluate the expression, generate the K-Element represented by the user, store if an assignment
        // Display           --> Convert the k-element to a string and display that to the end user.

    Scalar Symbols:
        Rules: Allow a single character, could also add these to a list.
        p,q,r,s   - points
        a,b,c,u,v - vectors
        A,B,C     - bivectors
        K,L,M,N   - line
        P,Q,R,S   - plane

        Then scalar symbols are P1 P2 P3 P4, any of the alphanumeric characters + an index at the end
        Which is a [u8|index] which is another u8 or a u16
*/


main :: () {

    USER_PROMPT :: "> ";
    success: bool = false;
    quit: bool    = false;

    lexer:       Lexer;
    parser:      Parser;
    interpreter: Interpreter;

    dimension: int = 4;

    update_exterior_space(dimension, .["e0","e1","e2","e3"]);
    init_global_parsing_tables();

	input: string;
	while !quit {
        

		print( "%", USER_PROMPT );
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

        tokens := generate_tokens(*lexer, input);
        if lexer.error_reported {
			print(lexer.error_message);
			continue;
		}

        ast := parse(*parser, tokens);
        descend(ast.root);

        reset_temporary_storage();
    }
		
}

#scope_file
#import "Basic";
#import "Hash_Table";