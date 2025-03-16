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
        A,B,C     - bivectors  (4 there are 6 of these basis) A1*e1|e2 + A2*e2|e3, A B C D E F E = E1e
        K,L,M,N   - line
        P,Q,R,S   - plane

        what you want to happen is that you enter e4 and end up with an undeclared identifier. Not a scalar symbol
        since it isn't defined on the basis set that you are dealing with. So you take those values and map them.
        It isn't ideal but that would work for the next time, how you do that is that it needs to fail to resolve
        as a scalar symbol. So then to match you have to be that character followed by at *most* 3 digits.

        p = e0 + p1*e1 + p2*e2 + p3*e3

        I think we could do that instead and have it work that way.
        So you declare a set of symbols and then you check if you make that list

        4  6  4
        1  2  3

        Then scalar symbols are P1 P2 P3 P4, any of the alphanumeric characters + an index at the end
        Which is a [u8|index] which is another u8 or a u16
*/

main :: () {

    USER_PROMPT :: "> ";
    success: bool = false;
    quit: bool    = false;

    lexer:  Lexer;
    tokens: [] Token;

    parser: Parser;
    ast:    Ast;

    default_dimension: int = 4;
    default_1_elements: [] string = .["e0","e1","e2","e3"];
    default_scalar_variable_prefix_characters: string = "abcdnpqrsuvABCKLMNPQRS";
    
    // Preconditions
    update_exterior_space(
         default_dimension
        ,default_1_elements
        ,default_scalar_variable_prefix_characters
    );
    
    initialize_operator_tables();

	input: string;
	while !quit {

        reset_temporary_storage();
        
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

    //    descend(ast.root);

        resolve_symbols(*ast, *space);
        descend(ast.root);

        pass := semantic_check(*ast);
        if !pass {
            continue;
        }
        
    }
		
}

#scope_file
#import "Basic";
#import "Hash_Table";