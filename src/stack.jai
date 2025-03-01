/*
    File implements a variable length Stack data structure.
    The 'Stack' instance always points to the last pushed element onto the stack or null when the stack is empty.

    Elements stored on the stack are shallow copies so the caller is responsible for freeing any memory associated
    with the pushed elements.
*/

#scope_export

Stack_Element :: struct (T: Type) {
    value: T;
    next: *Stack_Element(T);
}

Stack :: struct (T: Type) {
    count: int;
    head:  *Stack_Element(T);
}

stack_new :: ($T: Type) -> stack: *Stack(T) {

    // Allocates new stack instance whose elements are of type T.
    stack: *Stack(T) = alloc(size_of(Stack(T)));
    assert( stack != null );

    stack.count = 0;
    stack.head = null;

    return stack;
}

stack_is_empty :: (stack: *Stack($T)) -> bool {
    
    // Returns true if the stack contains no elements;
    return stack.count == 0;
}

stack_push :: (stack: *Stack($T), value: T) {

    // Add an element to the top of the Stack.
    // The last pushed element is the "head" of the Stack and will be returned on the next call to pop.
    // The element contains a shallow copy of the argument 'value' so non-primitive types that may be updated later in the client application
    // could unintentionally update the elements of the stack.

    assert(stack != null);

    element: *Stack_Element(T) = alloc(size_of(Stack_Element(T)));
    assert(element != null);

    element.value = value;
    element.next  = stack.head;

    stack.head = element;
    stack.count += 1;
}

stack_pop :: (stack: *Stack($T)) -> T {

    // If the Stack is non-empty, return the last pushed element.
    // If the Stack is empty, runtime assert and halt the program.

    assert(stack != null);
    assert(stack.count > 0, "Cannot pop an empty Stack");

    head  := stack.head;
    value := head.value;
    
    stack.head = head.next;
    free(head);

    stack.count -= 1;

    return value;
}

stack_peek :: (stack: *Stack($T)) -> T {

    // Returns the head of the stack. The caller should ensure the stack is not empty before calling.
    assert(stack != null);
    return stack.head.value;
    
}

stack_count :: (stack: *Stack($T)) -> int {
    
    // Return the number of elements currently in the Stack.

    assert(stack != null);
    return stack.count;
}

stack_deallocate :: (stack: **Stack($T)) {

    // Free the memory allocated provisioned for the stack elements and then the stack instance.
    // Called 'deallocate' to avoid name collision with the 'Basic' libraries 'free' function.
    assert((stack.*) != null);

    c := (stack.*).head;
    n := c;
    while c != null {
       n = c.next;
       free( c );
       c = n;
    }

    free((stack.*));

    (stack.*) = null; 
    return;
}

#scope_file
#import "Basic";