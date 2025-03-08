/*
    File implements a singly linked list data structure.

    Values stored within the list are shallow copies so the caller is responsible for freeing any memory associated
    with the values contained in the list.
*/

#scope_export

List_Node :: struct (T: Type) {
    value: T;
    next: *List_Node(T);
}

List :: struct (T: Type) {
    count: int            = 0;
    head:  *List_Node(T)  = null;
    tail:  *List_Node(T)  = null;
}

list_is_empty :: (list: *List($T)) -> bool {
    
    // Returns true if the list contains no elements.
    return list.count == 0;
}

list_append :: (list: *List($T), value: T) {

    // Add a node to the end of the list.
    // After the operation tail will point to this node.
    // The node contains a shallow copy of the argument 'value; so non-primitive types (like pointers) updated later in the
    // client application could unintentially update the node stored in the list.

    assert(list != null);

    node: *List_Node(T) = alloc(size_of(List_Node(T)));
    assert(node != null);

    node.value = value;
    node.next  = null;

    if list.count == 0 {
        list.head = node;
        list.tail = node;
    }
    else {
        list.tail.next = node;
        list.tail      = node;
    }

    list.count += 1;
}

list_prepend :: (list: *List($T), value: T) {

    // Add a node to the start of the list.
    // After the operation head will point to this node.
    // The node contains a shallow copy of the argument 'value; so non-primitive types (like pointers) updated later in the
    // client application could unintentially update the node stored in the list.

    assert(list != null);

    node: *List_Node(T) = alloc(size_of(List_Node(T)));
    assert(node != null);

    node.value = value;

    if list.count == 0 {
        list.head = node;
        list.tail = node;
        node.next = null;
    }
    else {
        node.next = head;
        head      = node;
    }

    list.count += 1;
}

list_head :: (list: List($T)) -> *List_Node(T) {
    
    // Assumes the list is non-empty and returns the address of the head node.
    assert(list.count != 0);
    return list.head;
}

list_tail :: (list: List($T)) -> *List_Node(T) {

    // Assumes the list is non-empty and returns the address of the tail node.
    assert(list.count != 0);
    return list.tail;
}

list_count :: (list: List($T)) -> int {
    
    // Return the number of nodes currently in the List.
    assert(list != null);
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