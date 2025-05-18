#scope_export

List :: struct (T: Type) {

    head: *List_Node(T);
    tail: *List_Node(T);
    count: int = 0;
}

List_Node :: struct (T: Type) {

    data: T;
    previous: *List_Node(T);
    next:     *List_Node(T);
}

append :: (list: *List($T), data: T) {

    assert(list != null);

    node := new_node(data);

    if list.count == 0 {
        list.head = node;
        list.tail = node;
    }
    else {
        list.tail.next = node;
        node.previous  = list.tail;
        node.next      = null;

        list.tail = node;
    }

    list.count += 1;
}

prepend :: (list: *List($T), data: T) {

    assert(list != null);

    node := new_node(data);

    if list.count == 0 {
        list.head = node;
        list.tail = node;
    }
    else {
        list.head.previous = node;
        node.previous = null;
        node.next = list.head;
        list.head = node;
    }

    list.count += 1;
}

find :: (list: *List($T), data: T) -> bool #must, *List_Node(T) {

    found: bool = false;
    result: *List_Node(T) = null;
    for :each_node node: list {
        if node.data == data {
            found = true;
            result = node;
            break;
        }
    }

    return found, result;
}

reverse :: (list: *List($T)) {

    temp: *List_Node(T);
    c := list.head;
    for i: 0..list.count-1 {
        temp       = c.previous;
        c.previous = c.next;
        c.next     = temp;

        c = c.previous; // swapped .previous and .next so this 'advances' us through the list.
    }

    // swap the head and the tail
    temp      = list.head;
    list.head = list.tail;
    list.tail = temp;

    return;
}

_remove :: (list: *List($T), data: T) -> bool {

    // Removes an item from the list if it exists.
    // Allows the user to supply a value which if found in the list is 'removed'/unlinked
    // 'remove' is a reserved keyword so using _remove for now.

    removed := false;

    found, node := find(list, data);
    if found {
        removed = unlink_node(list, node);
    }
   
    return removed;
}

_remove :: (list: *List($T), node: *List_Node(T)) -> bool {

    // Removes an item from the list if it exists.
    // Allows the user to skip the 'find' call if they already have a pointer to the list node.
    // 'remove' is a reserved keyword so using _remove for now.

    removed := unlink_node(list, node);
    return removed;
}

for_expansion :: (list: *List, body: Code, flags: For_Flags) #expand {

    // User-level traversal routine that support both forward and backward traversal.

    #if flags & .REVERSE {
        // Iterate from the end to the beginning of the list.
        c := list.tail;
        for i: 0..list.count-1 {
            `it_index := i;
            `it       := c.data;

            #insert body;
            c = c.previous;
        }
    }
    else {
        // Iterate from the start to the end of the list.
        c := list.head;
        for i: 0..list.count-1 {
            `it_index := i;
            `it       := c.data;

            #insert body;
            c = c.next;
        }
    }
}

test :: () {

    {
        // Zero element list
        list: List(int);
        found, node := find(*list,1);
        assert(found == false);

        reverse(*list);
        assert(list.count == 0 && list.head == null && list.tail == null);
    }

    {
        // One element list, append
        list: List(int);
        append(*list, 1);
        found, node := find(*list, 1);
        assert(found && node.data == 1);

        found, node = find(*list, 2);
        assert(found == false);

        reverse(*list);
        assert(list.count == 1 && list.head == list.tail);

        // remove does a find first

        removed := _remove(*list, 10);
        assert(removed == false);

        removed = _remove(*list, 1);
        assert(removed == true && list.count == 0 && list.head == null && list.tail == null);
    }

    {
        // One element list, prepend
        list: List(int);
        prepend(*list, 1);
        found, node := find(*list, 1);
        assert(found && node.data == 1);

        found, node = find(*list, 2);
        assert(found == false);

        reverse(*list);
        assert(list.count == 1 && list.head == list.tail);

        // remove does a find first

        removed := _remove(*list, 10);
        assert(removed == false);

        removed = _remove(*list, 1);
        assert(removed == true && list.count == 0 && list.head == null && list.tail == null);
    }

    {
        // Two element list
        list: List(int);
        append(*list, 1);
        append(*list, 2);

        found, node := find(*list, 1);
        assert(found && node.data == 1);

        found, node = find(*list, 2);
        assert(found && node.data == 2);

        // remove the head
        removed := _remove(*list, 1);
        assert(removed == true && list.count == 1 && list.head == node && list.tail == node);

        append(*list, 3);
        assert(list.head.data == 2 && list.tail.data == 3);

        // remove the tail
        removed = _remove(*list, 3);
        assert(removed == true && list.head.data == 2 && list.tail.data == 2);

        found, node = find(*list, 3);
        assert(found == false);

        append(*list, 10);

        assert(list.head.data == 2 && list.tail.data == 10);
        reverse(*list);
        assert(list.head.data == 10 && list.tail.data == 2);
    }

    {
        // N-element list
        list: List(int);

        append(*list,2);
        append(*list,3);
        append(*list,4);
        prepend(*list,1);

        assert(list.head.data == 1 && list.tail.data == 4);

        removed := _remove(*list, 3);
        assert(removed == true);

        found, node := find(*list, 3);
        assert(found == false);

        assert(list.head.data == 1 && list.head.next.data == 2 && list.tail.data == 4);

        for value: list { print("Value == %\n", value); }
        print("\n");

        reverse(*list);

        assert(list.head.data == 4 && list.head.next.data == 2 && list.tail.data == 1);
        for value: list { print("Value == %\n", value); }
        print("\n");
        
        // Reverse the list but print it backwards so forward =)
        reverse(*list);
        for < value: list { print("Value == %\n", value); }
        print("\n");
    }
}

#scope_file
#import "Basic";

each_node :: (list: *List, body: Code, flags: For_Flags) #expand {

    // Internal traveral over the list nodes themselves rather than the data.
    // Useful for 'find' and 'reversal' routine.
    
    #if flags & .REVERSE {
        // Backward traversal
        c := list.tail;
        for i: 0..list.count-1 {
            `it_index := i;
            `it       := c;

            #insert body;
            c = c.previous;
        }
    }
    else {
        // Forward traversal
        c := list.head;
        for i: 0..list.count-1 {
            `it_index := i;
            `it       := c;

            #insert body;
            c = c.next;
        }
    }
}

new_node :: (data: $T) -> *List_Node(T) {

    node := cast(*List_Node(T))alloc(size_of(List_Node(T)));
    assert(node != null);

    node.data     = data;
    node.previous = null; 
    node.next     = null;

    return node;
}

unlink_node :: (list: *List($T), node: *List_Node(T)) -> bool {
 
    unlinked := true;
    if list.count == 0 {
        // list is empty.
        unlinked = false;
    }
    else
    if list.count == 1 {
        // list contains single node.
        list.head = null;
        list.tail = null;
        list.count -= 1;
    }
    else
    if node == list.head {
        // adjust the head node
        list.head = node.next;
        list.head.previous = null;
        list.count -= 1;
    }
    else
    if node == list.tail {
        // adjust the tail node
        list.tail = node.previous;
        list.tail.next = null;
        list.count -= 1;
    }
    else {
        node.previous.next = node.next;
        node.next.previous = node.previous;
        list.count -= 1;
    }

    return unlinked;
}

// Get the size up front, the maximum size without reducing terms, then we could shrink to fit
// Relocatable_Array
//   -> data
//   -> capacity
//   -> whatever
// Relocatable_List
//  1 byte ->  character index
//  4 bytes -> index
//  2 bytes -> exponent
//  8
//  8 -> each one is 16 bytes
//
// If we delete then do we have to maintain the order? not really
// We could just