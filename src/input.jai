#scope_export

read_line :: () -> ( success: bool, user_input: string ) {

	// Read line of user input from the terminal or console.
    success: bool = true;
	user_input: string;
	user_input.data = user_input_buffer.data;

    if !std_in.handle {
		std_in.handle = GetStdHandle( STD_INPUT_HANDLE );
	}

    bytes_read: u32 = 0;
    ok := ReadFile( std_in.handle, user_input_buffer.data, cast(u32)USER_INPUT_BUFFER_SIZE, *bytes_read, null );
    if ok && bytes_read > 0 {
		if user_input.data[bytes_read-1] == #char "\n" {
            user_input.count = bytes_read - CARRIAGE_RETURN_AND_NEWLINE;
            success = true;
		}
        else {
            print( "Expression length exceeded limit of % characters, ignoring input\n", MAX_USER_INPUT_LENGTH );
            success = false;
        }
    }
    else {
        print("Failed to read the standard input stream successfully.");
        success = false;
    }

	return success, user_input;
}

flush_input :: () -> () {

    if !std_in.handle {
        print("Unable to flush standard input stream. No active handle to the file input stream.\n");
        return;
    }
    
    // Read individual characters from stdin until we hit the newline character.
	temp: u8;
    bytes_read: u32 = 0;
	while true {
	    ok := ReadFile( std_in.handle, *temp, 1, *bytes_read, null );
		if temp == #char "\n" { break; }
	}
}

#scope_file

#import "Windows";
#import "File";

// Input buffer size is arbitrary and should be adjusted later to accommodate larger expressions.
MAX_USER_INPUT_LENGTH		:: 100;
CARRIAGE_RETURN_AND_NEWLINE :: 2;
USER_INPUT_BUFFER_SIZE 		:: MAX_USER_INPUT_LENGTH + CARRIAGE_RETURN_AND_NEWLINE;

user_input_buffer: [USER_INPUT_BUFFER_SIZE] u8;
std_in: File;