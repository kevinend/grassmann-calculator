#scope_export

Factor :: struct {
    // Represents a single scalar factor like 'a1' on a basis term 'a1^e1'
    // The exponent determines whether it is a numerator (positive) or denominator (negative).
    exponent: s32;
    encoded_symbol: u32;
}

new_factor :: (exponent: s32, symbolic_factor: string, heap: *Bump_Heap) -> *Factor {

    factor := cast(*Factor)heap_allocate(heap, size = size_of(Factor), alignment = size_of(Factor));
    assert(factor != null);
    memset(factor, 0, size_of(Factor));

    factor.exponent = exponent;
    factor.encoded_symbol = encode(symbolic_factor);

    return factor;
}

print_factor :: (factor: *Factor) -> string {
    // stub
    return "";
}

matches_symbolic_factor :: (ident: string, symbolic_factor_prefix_characters: string) -> bool {

    // Symbolic factors are identifiers that have a leading character in the 'allowed-list' for symbolic factors and are followed by at least 1 digit.
    matching_character_prefix: bool = false;
    for i: 0..symbolic_factor_prefix_characters.count-1 {
        if ident[0] == symbolic_factor_prefix_characters[i] {
            matching_character_prefix = true;
            break;
        }
    }

    followed_by_digit: bool = false;
    if ident.count > 1 && is_digit(ident[1]) {
        followed_by_digit = true;
    }

    return matching_character_prefix && followed_by_digit;
}

is_valid_symbolic_factor :: (ident: string) -> bool {

    // Symbolic factors are identifiers that have a leading character in the 'allowed-list' and be followed by at *most* two digits.
    return ident.count <= 3;
}

#scope_file

encode :: (ident: string) -> u16 {

    // Encodes a scalar variable like 'a1' or 'a11' into a 16-bit integer in the following format:
    //                 byte1                byte2
    //       -----------------------| ---------------
    // u16 = [character  num-digits | digit1  digit2]
    //        ^6-bits    ^2-bits     ^4-bits  ^4-bits
    //

    assert(ident.count == 2 || ident.count == 3);

    c := cast(u16)(ident[0] - #char "A"); // A = 65, z = = 122, diff = 58 which can be represented in 6 bits
    n := cast(u16)ident.count - 1;        // n is the number of digits following the character.

    value: u16;
    value |= c << 10;
    value |= n <<  8;

    if n == 2 {
        value |= cast(u16)(ident[1] - #char "0") << 4;
        value |= cast(u16)(ident[2] - #char "0") << 0;
    }
    else
    if n == 1 {
        value |= cast(u16)(ident[1] - #char "0") << 4;
    }

    return value;
}

decoder: String_Builder;
decode :: (value: u16) -> string {

    // Decodes a 16-bit integer like 1296 into the scalar symbol 'B1'
    // Encoding procedure describes the format of the encoding.

    reset(*decoder);
    append(*decoder, cast(u8)((value >> 10) & 0x3F) + #char "A");

    count := cast(u8)((value >> 8) & 0x03);

    if count == 2 {
        append(*decoder, cast(u8)((value >> 4) & 0xF) + #char "0");
        append(*decoder, cast(u8)((value >> 0) & 0xF) + #char "0");
    }
    else
    if count == 1 {
        append(*decoder, cast(u8)((value >> 4) & 0xF) + #char "0");
    }

    return builder_to_string(*decoder);
}
