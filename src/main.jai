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

    USER_PROMPT :: "> ";
    success: bool = false;
    quit: bool    = false;

    lexer:  Lexer;
    tokens: [] Token;

    parser: Parser;

    ast:            Ast;
    expanded_ast:   Ast;
    simplified_ast: Ast;

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

        expanded_ast = expand(ast);
        descend(expanded_ast.root);

        simplified_ast = simplify(expanded_ast);
        descend(simplified_ast.root);
    }
}

#scope_file
#import "Basic";
#import "Hash_Table";