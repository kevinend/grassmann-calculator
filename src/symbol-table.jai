#scope_export

Symbol_Table :: struct {
    keywords:       Table(string, string);    // interned keywords
    scalar_symbols: Table(string, string);    // interned scalar symbols
    symbols:        Table(string, *Ast_Node); // results of an expression
} 