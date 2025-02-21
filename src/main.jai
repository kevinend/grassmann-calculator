#scope_export


main :: () {
   
    USER_PROMPT :: "> ";
    success: bool = false;
    quit: bool    = false;

	user_input: string;
	while !quit {
        reset_temporary_storage();

		print( "%", USER_PROMPT );
		success, user_input = read_line();
        if !success {
            flush_input();
            continue;
        }
        else
        if user_input == "quit" {
            quit = true;
        }
    }
		
}

#scope_file
#import "Basic";