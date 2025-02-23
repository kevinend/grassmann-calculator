#scope_export

main :: () {
   
    USER_PROMPT :: "> ";
    success: bool = false;
    quit: bool    = false;

    lexer: Lexer;

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

        generate_tokens(*lexer, input);
        if lexer.error_reported {
			print(lexer.error_message);
			continue;
		}
    }
		
}

#scope_file
#import "Basic";