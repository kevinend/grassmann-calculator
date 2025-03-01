#scope_export

main :: () {

    USER_PROMPT :: "> ";
    success: bool = false;
    quit: bool    = false;

    lexer:       Lexer;
    parser:      Parser;
    interpreter: Interpreter;

    update_exterior_space(*interpreter.exterior_space, dimension = 4, one_element_symbols = .["e0","e1","e2","e3"]);
    init_global_parsing_tables();

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
            print( "No input; Enter a valid Grassmann expression or 'quit' to quit\n" );
            continue;
        }
        else
        if input == "quit" {
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
    }
		
}

#scope_file
#import "Basic";
#import "Hash_Table";