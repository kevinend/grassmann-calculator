/*
	The lexer takes user input and turns it into a list of tokens. 
	The tokens are supplied to the parser which converts them into an Abstract Syntax Tree.

	The lexer can and will report errors immediately. This makes error reporting split across different
	parts of the application. However, having the semantic pass report all errors can cause issues since
	garbage out of the lexer pollutes the parser then the AST so that errors can be completely
	mangled by the time they are finally reported on.
*/

#scope_export

Lexer :: struct {

    input: string;

    // Maintain current location in the input stream.
    cursor:          int = 0;
    line_number:     int = 0;
    character_index: int = 0;

    MAX_TOKENS_PER_EXPRESSION :: 100;

    tokens: [MAX_TOKENS_PER_EXPRESSION] Token;
    num_tokens: int = 0;

	token_position: Position_Info;

	TEMP_BUFFER_SIZE :: 64; // dictates the *max* size of identifiers and the number of digits in a number.

	temp_buffer: [TEMP_BUFFER_SIZE] u8;

    error_detected: bool;
    error_message:  string;
}

generate_tokens :: (lexer: *Lexer, input: string) -> bool #must, [] Token {

    reset_lexer(lexer, input);

    c: u8;
	success: bool;
	token: Token;

    while true && !lexer.error_detected {

        c = peek_next_character(lexer);
        while c && is_space(c) {
            consume_character(lexer);
            c = peek_next_character(lexer);
        }

        if !c {
            // consumed all available input
            token = compose_token_end_of_input(lexer);
			add_token(lexer, token);
			break;
        }

        tag_token_start(lexer);

        if begins_literal(c) {
			token = compose_token_literal(lexer);
			add_token(lexer, token);
			continue;
		}

		if begins_number(c) {
			token = compose_token_number(lexer);
			add_token(lexer, token);
			continue;
		}

		if begins_identifier(c) {
			token = compose_token_identifier_or_keyword(lexer);
			add_token(lexer, token);
			continue;
		}

        token = compose_token_operator_or_separator(c, lexer);
		add_token(lexer, token);
    }

    return !lexer.error_detected, lexer.tokens;
}

get_lexer_error :: (lexer: *Lexer) -> string {
	return lexer.error_message;
}

#scope_file

reset_lexer :: (lexer: *Lexer, input: string) {

    lexer.error_detected = false;

    lexer.input = input;

    lexer.cursor = 0;
	lexer.num_tokens = 0;

    // Assumes the input is still coming from the console. If it comes from a file then we need
    // to keep track of the current line number and not reset character index on every iteration of the loop!
    lexer.line_number     = 0;
    lexer.character_index = 0;

    lexer.token_position.line_number = 0;
    lexer.token_position.char_start  = 0;
}

peek_next_character :: inline (lexer: *Lexer) -> u8 {

	if (lexer.input.count - lexer.cursor) > 0 {
		return lexer.input[lexer.cursor];
	}
	
	return 0;
}

consume_character :: inline (lexer: *Lexer) {

	if lexer.input[lexer.cursor] == #char "\n" { lexer.line_number += 1; }

	lexer.cursor += 1;
	lexer.character_index += 1;
}

flag_error :: (lexer: *Lexer, message: string) {

	// Lexer errors are for unrecognized character sequences that we can't turn into tokens.
	// Error messages are written to temporary storage, will be wiped on each iteration of the program.

	line_number := lexer.token_position.line_number;
	char_start  := lexer.token_position.char_start;

	base := tprint("Lexer Error! Line:%, Char:% -- ", line_number, char_start);

	builder: String_Builder;
	builder.allocator = temp;

	append(*builder, base);
	append(*builder, message);

	lexer.error_detected = true;
	lexer.error_message  = builder_to_string(*builder);
}

add_token :: (lexer: *Lexer, token: Token) {

	assert(lexer.num_tokens < Lexer.MAX_TOKENS_PER_EXPRESSION);

	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;
}

tag_token_start :: (lexer: *Lexer) {

    lexer.token_position.line_number = lexer.line_number;
	lexer.token_position.char_start  = lexer.character_index;
}

tag_token_end :: inline (lexer: *Lexer, token: *Token) {

    token.position.line_number = lexer.token_position.line_number;
    token.position.char_start  = lexer.token_position.char_start;
    token.position.char_end    = lexer.character_index-1;
}

error_token :: () -> Token {

	token: Token;
	token.type = .Error;

	return token;
}

begins_literal :: inline (c: u8) -> bool {
	return c == #char "'";
}

compose_token_literal :: (lexer: *Lexer) -> token: Token {

	continues_literal :: (c: u8) -> bool {
		return c != #char "'";
	}

	success: bool;
	c: u8;

	// alias temporary buffer; copy literal (excluding quotes) into token
	t: [] u8;
	t.data  = lexer.temp_buffer.data;
	t.count = 0;

	consume_character(lexer); // initial single quote
	while true {
		if t.count >= Lexer.TEMP_BUFFER_SIZE {
            flag_error(lexer, "The current literal exceeds the maximum allowed length.\n");
			return error_token();
		}

		c = peek_next_character(lexer);
		if c && continues_literal(c) {
			t.data[t.count] = c;
			t.count += 1;
			consume_character(lexer);
		}
		else {
			break;
		}
	}

	if c == #char "'" {
		consume_character(lexer);
	}
	else {
		flag_error(lexer, "String literal is missing ending single quote.\n");
		return error_token();
	}

	token: Token;
	token.type = .Literal;
	token.text.data = talloc(t.count * size_of(u8));
	memcpy(token.text.data, t.data, t.count);
	token.text.count = t.count;

	tag_token_end(lexer, *token);

	return token;
}

begins_number :: (c: u8) -> bool {
	return is_digit(c);
}

compose_token_number :: (lexer: *Lexer) -> token: Token {

	// This procedure can get very complicated trying to handle floats, hex, overflow, etc...
	// As a first pass just handle integers.
	// Convert the char representation of the numbers to integers as base 10 and store in a float.

	continues_number :: inline (c: u8) -> bool {
		return is_digit(c) || c == #char "_";
	}

	power :: (a: int, b: int) -> int {

		c := 1;
		for i: 0..b-1 { c *= a;	}

		return c;
	}

	success: bool;
	c: u8;

	t: [] u8;
	t.data  = lexer.temp_buffer.data;
	t.count = 0;

	while true {
		if t.count > Lexer.TEMP_BUFFER_SIZE {
			flag_error(lexer, "Maximum number of digits exceeded\n");
			return error_token();
		}

		c = peek_next_character(lexer);
		if c && continues_number(c) {
			consume_character(lexer);
			if c == #char "_" { 
				continue; 
			}
			t.data[t.count] = c - #char "0";
			t.count += 1;
		}
		else {
			break;
		}
	}

	// Pushed all the digits onto the temp buffer (converting from ascii to their integer values).
	// Walk the temp buffer and digits and build final number.
	// Ex. 123 --> 1*(10^2) + 2*(10^1) + 1*(10^0)

	base: int = 10;
	exponent: int = t.count-1;
	sum: float = 0;
	for i: 0..t.count-1 {
		sum += (t.data[i] * power(base, exponent));
		exponent -= 1;
	}

	token: Token;
	token.type  = .Number;
	token.value = sum; 

	tag_token_end(lexer, *token);

	return token;
}

begins_identifier :: inline (c: u8) -> bool {
	return is_alpha(c) ||  c == #char "_";
}

compose_token_identifier_or_keyword :: (lexer: *Lexer) -> token: Token {

	continues_identifier :: inline (c: u8) -> bool {
		return is_alnum(c);
	}

	check_for_keyword :: (ident: string) -> Token_Type {

		type := Token_Type.Undefined;
 		if ident == {
			case "dir";   { type = .Keyword_Direction; }
			case "pos";   { type = .Keyword_Position;  }
			case "basis"; { type = .Keyword_Basis;     }
		}

		return type;
	}

	success: bool;
	c: u8;

	// alias temporary buffer; copy identifier into token once processed
	t: string;
	t.data  = lexer.temp_buffer.data;
	t.count = 0;

	c = peek_next_character(lexer);
	t.data[0]  = c; // using .data to ignore array bounds checking
	t.count   += 1;
	consume_character(lexer);

	while true {
		if t.count > Lexer.TEMP_BUFFER_SIZE {
			flag_error(lexer, "Identifer exceeded maximum length.");
			return error_token();
		}

		c = peek_next_character(lexer);
		if c && continues_identifier(c) {
			t.data[t.count] = c; // .data to ignore array bounds checking
			t.count += 1;
			consume_character(lexer);
		}
		else { break; }
	}

    token: Token;
	
	keyword := check_for_keyword(t);
	if keyword {
		token.type = keyword;
	}
	else {
		token.type = .Identifier;
		token.text.data = talloc(t.count);
		memcpy(token.text.data, t.data, t.count);
		token.text.count = t.count;
	}

    tag_token_end(lexer, *token);
    return token;
}

compose_token_operator_or_separator :: (c: u8, lexer: *Lexer) -> token: Token {

    // The meaning of the symbols is handled by the parser.
    // For example, '-' is the token 'Minus' because in the prefix position it is Negation whereas
    // as an infix operator it means substraction.

	consume_character(lexer);

	token: Token;
	if c == {
		// Operators
        case #char "="; { token.type = .Equals;             }
		case #char "+"; { token.type = .Plus;               }
		case #char "-"; { token.type = .Minus;              }
		case #char "*"; { token.type = .Asterisk;           }
		case #char "/"; { token.type = .Forward_Slash;      }
        case #char "^"; { token.type = .Caret;              }
        case #char "|"; { token.type = .Pipe;               } // vertical-bar
        case #char "&"; { token.type = .Ampersand;          }
        case #char "!"; { token.type = .Exclamation_Point;  }
		case #char "@"; { token.type = .At_Symbol;          }

		// Separators/Grouping
        case #char "("; { token.type = .Open_Paren;   }
        case #char ")"; { token.type = .Closed_Paren; }
        case #char ","; { token.type = .Comma;        }
        case #char ";"; { token.type = .Semicolon;    }

		case; {
			builder: String_Builder;
			append(*builder, "Unrecognized operator or separator '");
			append(*builder, c);
			append(*builder, "'\n");
			message := builder_to_string(*builder);
			flag_error(lexer, message);
			return error_token();
		}
	}

    
	tag_token_end(lexer, *token);

	return token;
}

compose_token_end_of_input :: (lexer: *Lexer) -> (token: Token) {

    // append a semicolon at the end of each line of input to indicate the end of the expression.
	token: Token;
	token.type = .Semicolon;

	return token;
}