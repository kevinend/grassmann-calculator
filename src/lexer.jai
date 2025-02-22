#scope_export

Lexer :: struct {

    input: string;

    cursor:          int = 0;
    line_number:     int = 0;
    character_index: int = 0;

    MAX_TOKENS_PER_EXPRESSION :: 100;  // an expression is a single line of input
    tokens: [MAX_TOKENS_PER_EXPRESSION] Token;
    num_tokens: int = 0;

    token_line_number:     int = 0;
    token_character_index: int = 0;
    
	TEMP_BUFFER_SIZE :: 64; // dictates the size of identifiers and the number of digits in a number
	temp_buffer: [TEMP_BUFFER_SIZE] u8;
}

generate_tokens :: (lexer: *Lexer, input: string) -> [] Token {

    reset_lexer(lexer, input);

    c: u8;
	success: bool;
	token: Token;

    while true {
        success, c = peek_next_character(lexer);
        while success && is_space(c) {
            consume_character(lexer);
            success, c = peek_next_character(lexer);
        }

        if !success {
            // consumed all available input
            token = compose_end_of_input_token(lexer);
			add_token( lexer, token );
			break;
        }

        consume_character(lexer);
    }

    return lexer.tokens;
}

#scope_file

reset_lexer :: (lexer: *Lexer, input: string) {

    lexer.input = input;

    lexer.cursor = 0;

    lexer.token_line_number = 0;
    lexer.token_character_index = 0;
}

peek_next_character :: inline (lexer: *Lexer) -> (success: bool, c: u8) {
	
	if (lexer.input.count - lexer.cursor) > 0 { 
		return true, lexer.input[lexer.cursor];
	}
	else {
		return false, 0;
	}
}

consume_character :: inline (lexer: *Lexer) {

	if lexer.input[lexer.cursor] == #char "\n" { lexer.line_number += 1; }

	lexer.cursor += 1;
	lexer.character_index += 1;
}

compose_end_of_input_token :: (lexer: *Lexer) -> (token: Token) {

    // append a semicolon at the end of each line of input to indicate the end of the expression.
	token: Token;
	token.type = .Semicolon;

	return token;
}

add_token :: (lexer: *Lexer, token: Token) {

	assert(lexer.num_tokens < Lexer.MAX_TOKENS_PER_EXPRESSION );
	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;

	return;	
}