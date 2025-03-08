/*
	The lexer takes user input and turns it into a list of tokens. 
	The tokens are supplied to the parser which converts them into an Abstract Syntax Tree.

	Note: Identifiers are converted to one of four token types.
	1. Identifiers
	2. Keywords
	3. Basis_Elements
	4. Scalars (symbolic scalars like 'a1', b2', etc)

	(1) and (2) are common in a lexer but (3) and (4) are not. 
	Traditionally if a language supports things like variables, functions, etc that are all 'identifiers' of some
	sort then those get converted into an 'Identifier' token and resolved later as a pass over the AST when
	more context (other nodes) are available.

	(3) and (4) were initially implemented in the way described above as part of a 'symbol-resolution' pass
	over the AST. However, we don't need additional context for what a basis element is. If it matches one of the
	currently declared 1-elements then it is a basis element. We don't have to wait for another time in the application
	to do this comparison so might as well do it as early as possible which is here in the lexer.

	Scalars aren't as straight-forward. They could be implemented as context-sensitive identifiers such as those used with
	'scalar operations' like scalar-multiplication or scalar-addition. This would require the implementation described above
	where the lexer generates an 'Identifier' token and a separate pass over the AST resolves it to a scalar based on its location in the AST.

	For this current iteration of the application a strict selection criteria/pattern is imposed on scalar symbols. 
	They are defined as a single ASCII character 'a-z' or 'A-Z' followed by a maximum of a 3 digit index such as 'a1', 'a01', or 'a001'.
	This means that during lexing we *can* identify if an identifier is just a scalar symbol. Whether it is used in a valid location
	(such as part of a scalar multiplication) can be determined as part of a semantic pass over the AST.
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

    // Track the current token being lexed. Update lexer state on new token and on completion of token and then update metadata on token instance.
    token_line_number:           int = 0;
    token_character_index_start: int = 0;

	TEMP_BUFFER_SIZE :: 64; // dictates the size of identifiers and the number of digits in a number

	temp_buffer: [TEMP_BUFFER_SIZE] u8;

    error_reported: bool;
    error_message:  string;
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

		if begins_scalar(c) {
			token = compose_token_scalar(lexer);
			add_token(lexer, token);
			continue;
		}

		if begins_identifier(c) {
			token = compose_token_identifier_keyword_basis_element_or_scalar(lexer);
			add_token(lexer, token);
			continue;
		}

        token = compose_token_operator_or_separator(c, lexer);
		add_token(lexer, token);
    }

    return lexer.tokens;
}

#scope_file

reset_lexer :: (lexer: *Lexer, input: string) {

    lexer.error_reported = false;

    lexer.input = input;

    lexer.cursor = 0;
	lexer.num_tokens = 0;

    // Assumes the input is still coming from the console. If it comes from a file then we need
    // to keep track of the current line number and not reset character index on every iteration of the loop!
    lexer.line_number     = 0;
    lexer.character_index = 0;

    lexer.token_line_number           = 0;
    lexer.token_character_index_start = 0;
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

report_error :: (lexer: *Lexer, message: string) {

	// Lexer errors are for unrecognized character sequences that we can't turn into tokens.
	// Error messages are written to temporary storage, will be wiped on each iteration of the program.
	default_message := tprint( "Lexing error occurred on line number %, character %\n", lexer.token_line_number, lexer.token_character_index_start );

	error_message: string;
	error_message.count = default_message.count + message.count;
	error_message.data  = talloc( error_message.count );
	assert( error_message.data != null );

	memcpy( error_message.data, default_message.data, default_message.count );
	memcpy( error_message.data + default_message.count, message.data, message.count );

	lexer.error_reported = true;
	lexer.error_message  = error_message;
}

add_token :: (lexer: *Lexer, token: Token) {

	assert(lexer.num_tokens < Lexer.MAX_TOKENS_PER_EXPRESSION );

	lexer.tokens[lexer.num_tokens] = token;
	lexer.num_tokens += 1;
}

tag_token_start :: (lexer: *Lexer) {

    lexer.token_line_number 	       = lexer.line_number;
	lexer.token_character_index_start  = lexer.character_index;
}

tag_token_end :: inline (lexer: *Lexer, token: *Token) {

    token.line_number           = lexer.token_line_number;
    token.character_index_start = lexer.token_character_index_start;
    token.character_index_end   = lexer.character_index-1;
}

error_token :: () -> (token: Token) {

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
            report_error( lexer, "The current literal exceeds the maximum allowed length.\n" );
			return error_token();
		}

		success, c = peek_next_character(lexer);
		if success && continues_literal(c) {
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
		report_error(lexer, "Literal is missing ending single quote.\n");
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

begins_scalar :: (c: u8) -> bool {
	return is_digit(c);
}

compose_token_scalar :: (lexer: *Lexer) -> token: Token {

	// This procedure can get very complicated trying to handle floats, hex, overflow, etc...
	// As a first pass just handle integers.
	// Convert the char representation of the numbers to integers as base 10 and store in a float.

	continues_scalar :: inline (c: u8) -> bool {
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
			report_error(lexer, "Maximum number of digits exceeded\n");
			return error_token();
		}

		success, c = peek_next_character(lexer);
		if success && continues_scalar(c) {
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
	token.type   = .Scalar;
	token.scalar = .{ value = sum, exponent = 1, symbol_hash = hash_scalar_symbol("1") };

	tag_token_end(lexer, *token);

	return token;
}

begins_identifier :: inline (c: u8) -> bool {
	return is_alpha(c) ||  c == #char "_";
}

compose_token_identifier_keyword_basis_element_or_scalar :: (lexer: *Lexer) -> token: Token {

	continues_identifier :: inline (c: u8) -> bool {
		return is_alnum(c);
	}

	check_for_keyword :: (ident: string) -> Token_Type {

		type: Token_Type = .Undefined;

 		if ident == {
			case "dir";   { type = .Keyword_Direction; }
			case "pos";   { type = .Keyword_Position;  }
			case "basis"; { type = .Keyword_Basis;     }
		}

		return type;
	}

	check_for_0_element :: (ident: string) -> u16 {

		// Single character identifiers or identifiers with leading alpha but more than 3 indices are *not* valid symbolic scalars.
		// Each would just return an identifier, which seems a bit odd that 'a001' would be a symbolic scalar but 'a0001' would not.
		// Maybe have the lexer report an error that symbolic scalars must be in range 0-999?

		hash: u16 = 0;
		if ident.count == 1 || ident.count > 4 {
			return hash;
		}

		c := ident[0];
		if (c >= #char "a" && c <= #char "z") || (c >= #char "A" && c <= #char "Z") {
			m := 1;
			for i: 1..ident.count-1 {
				if is_digit(ident[i]) { m += 1; }
			}

			if m == ident.count {
				hash = hash_scalar_symbol(ident);
			}
		}

		return hash;
	}

	check_for_1_element :: (ident: string) -> *Basis_Element {

		element: *Basis_Element = null;

		for i: 1..space.dimension {
			if ident == space.basis_elements[i].symbol {
				element = *space.basis_elements[i];
				break;
			}
		}

		return element;
	}

	success: bool;
	c: u8;

	// alias temporary buffer; copy identifier into token once processed
	t: string;
	t.data  = lexer.temp_buffer.data;
	t.count = 0;

	success, c = peek_next_character(lexer);
	t.data[0]  = c; // using .data to ignore array bounds checking
	t.count   += 1;
	consume_character(lexer);

	while true {
		if t.count > Lexer.TEMP_BUFFER_SIZE {
			report_error(lexer, "Identifer exceeded maximum length.");
			return error_token();
		}

		success, c = peek_next_character(lexer);
		if success && continues_identifier(c) {
			t.data[t.count] = c; // .data to ignore array bounds checking
			t.count += 1;
			consume_character(lexer);
		}
		else { break; }
	}

    token: Token;
	builder: String_Builder;
	while true {
		_1_element := check_for_1_element(t);
		if _1_element {
			token.type  = .Basis_Element;
			token.basis = _1_element;
			break;
		}

		hash := check_for_0_element(t);
		if hash {
			token.type   = .Scalar;
			token.scalar = .{ exponent = 1, value = 1, symbol_hash = hash };

			append(*builder,t);
			text := builder_to_string(*builder);
			insert_scalar_symbol(*space.scalar_symbols, hash, text);

			break;
		}

		keyword := check_for_keyword(t);
		if keyword {
			token.type = keyword;
			break;
		}

		token.type = .Identifier;
		token.text.data = talloc(t.count);
		memcpy(token.text.data, t.data, t.count);
		token.text.count = t.count;

		break;
	}

    tag_token_end(lexer, *token);
    return token;
}

compose_token_operator_or_separator :: (c: u8, lexer: *Lexer) -> token: Token {

    // The meaning of the symbols is handled by the parser.
    // For example, '-' is the token 'Minus' because in the prefix position it is Negation whereas
    // as an infix operator it means substraction.

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
			report_error( lexer, "Unrecognized operator or separator." );
			return error_token();
		}
	}

    consume_character(lexer);
	tag_token_end(lexer, *token);

	return token;
}

compose_token_end_of_input :: (lexer: *Lexer) -> (token: Token) {

    // append a semicolon at the end of each line of input to indicate the end of the expression.
	token: Token;
	token.type = .Semicolon;

	return token;
}