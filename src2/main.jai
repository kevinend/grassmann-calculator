

// The expression heap stores all basis-term info during the evaluation of a single expression.
// This includes standard representations of zero and one, all coefficient, terms and factors for all AST nodes as well
// as intermediate calculations.
// Once evaluation is complete the heap is reset to be available to work on the next expression.
#add_context expression_heap: *Bump_Heap;
#add_context exterior_space:  *Exterior_Space;

main :: () {

    // Context/Globals
    expression_heap: Bump_Heap;
    context.expression_heap = *expression_heap;
    initialize_standard_basis_terms();
    heap_set_checkpoint(*expression_heap);

    exterior_space: Exterior_Space;
    context.exterior_space = *exterior_space;
    default_1_elements: [] string = .["e0","e1","e2","e3"];
    init(*exterior_space, default_1_elements);

    initialize_operator_tables();

    // Variables for the main loop
    quit:    bool = false;
    success: bool = false;

    user_prompt: string = "> ";
    input: string;

    lexer: Lexer;
    tokens: [] Token;

    parser: Parser;
    ast:    Ast;

    heap_reset(*expression_heap);
}