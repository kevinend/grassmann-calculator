#scope_export

// Idea: Use the symbol table to hold the basis elements!
//  when you create one you create a new basis_element node that is pre-populated
//  you do the same for the scalar symbols so you return a bunch of identifiers then you do a lookup during parsing
//  the list of available scalar symbols as well, you don't need to know that ahead of time, but you do need to know at parse time...
//  this might simplify things because i tried to do that in the lexer, which wasn't helpful
//  Identifier or Keyword and that is about it
//   then in parsing we try to lookup the identifier, if it is a basis element then replace with a basis element node
//   

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