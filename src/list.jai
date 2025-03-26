/*
    File implements a singly linked list data structure.

    Constraints:
    (1) Elements *must* be pointer types.
    (2) List element *must* be an instrusive data type that contains an internal .next pointer.

    Values stored within the list are shallow copies so the caller is responsible for freeing any memory associated
    with the values contained in the list.
*/
 
#scope_export

/*
    What is the plan stan?
    Put the next pointer on the thingy, take off the previous and keep a count, who cares if we iterate over it again
    Then we change List(Term) to a coefficient: List(Term) and a List(Ast_Node)
    Then we get back and use the .data field directly instead of using a wrapper
    Which are themselves linked list so then we can say

    list_next(T: Type) -> T
    AST Node though. we return a pointer to it, node.next -->

    do we need the list_node is what I am saying?
    or does this store intrusive data structures?

    list_next(T:Type) -> *term -> *term
    *Ast_Node -> *Ast_Node

    List(*Term) --> you get back a pointer to a term
    then you can say next_term(current) --> next pointer and gives you it back
    
    Lterm := list_head(node.args);
    for i: 0..count-1 {
        Lterm = next_term(Lterm);
    }
*/

List :: struct (T: Type) {
    count: int = 0;
    head:  T;
    tail:  T;
}
 
list_is_empty :: inline (list: List($T)) -> bool {
    // Returns true if the list contains no elements.
    return list.count == 0;
}
 
list_append :: (list: *List($T), value: T) {

    // Adds the new to the end of the list.
    assert(list  != null);

    if list.count == 0 {
        list.head = value;
        list.tail = value;
    }
    else {
        list.tail.next = value;
        list.tail      = value;
    }

    list.count += 1;
}
 
list_prepend :: (list: *List($T), value: T) {

    // Add a node to the start of the list.
    assert(list != null);
 
    if list.count == 0 {
        list.head = node;
        list.tail = node;
    }
    else {
        value.next = head;
        list.head  = value;
    }

    list.count += 1;
}

list_head :: (list: List($T)) -> T {
    // Assumes the list is non-empty and returns the address of the head node.
    assert(list.count != 0);
    return list.head;
}
 
list_tail :: (list: List($T)) -> T {
    // Assumes the list is non-empty and returns the address of the tail node.
    assert(list.count != 0);
    return list.tail;
}

list_next :: inline (current: $T) -> T {
    // Assumes a pointer type with a .next pointer!
    assert(current != null);
    return current.next;
}

list_count :: (list: List($T)) -> int {
    // Return the number of nodes currently in the List.
    return list.count;
}
 
list_deallocate :: (list: *List($T)) {
 
    // Free the memory allocated provisioned for the list nodes and then the list instance.
    // Called 'deallocate' to avoid name collision with the 'Basic' libraries 'free' function.

    assert(list != null);
    c := list.head;

    n := c;
 
    while c != null {
       n = c.next;
       free(c);
       c = n;
    }
 
    list = null;
    return;
}

#scope_file
#import "Basic";