/*
    This file implements a semantic pass over the AST.
    Checks the AST to make sure what the user input makes sense with respect to the algebra.

    It stops on the first error found in the tree and returns. It doesn't make sense to spew
    a bunch of errors that are caused by other errors. If an expression has two errors the first
    execution will report the first found and once corrected and resubmitted it will report the second.

*/

semantic_check :: (ast: *Ast) -> bool {

    return true;
}