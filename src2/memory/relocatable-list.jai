
#scope_export

// Relocatable_List *must* be initialized with a custom allocator so that allocations
// restricted to within a certain memory boundary, otherwise the relative pointers/offsets could be invalid.

Relocatable_List :: struct (T: Type) {

    is_initialized: bool = false;

    head: Relative_Pointer(Relocatable_List_Node(T));
    tail: Relative_Pointer(Relocatable_List_Node(T));
    count: int = 0;

    allocator: Allocator;
}

Relocatable_List_Node :: struct (T: Type) {
    
    data: T;
    previous: Relative_Pointer(Relocatable_List_Node(T));
    next:     Relative_Pointer(Relocatable_List_Node(T));
}

init :: (list: *Relocatable_List($T), allocator: Allocator) {

    list.is_initialized = true;

    set(*list.head, null);
    set(*list.tail, null);
    list.count = 0;
    list.allocator = allocator;

    return;
}

append :: (list: *Relocatable_List($T), data: T) {

    assert(list != null);
    
    if !list.is_initialized {
        init(list, context.allocator);
    }

    push_allocator(list.allocator);
    node := new_relocatable_node(data);
    
    if list.count == 0 {
        set(*list.head, node);
        set(*list.tail, node);
    }
    else {
        tail := get(list.tail);
        set(*tail.next, node);

        set(*node.previous, tail);
        set(*node.next,     null);

        set(*list.tail, node);
    }

    list.count += 1;
}

for_expansion :: (list: *Relocatable_List, body: Code, flags: For_Flags) #expand {

    // User-level traversal routine that support both forward and backward traversal.

    #if flags & .REVERSE {
        // Iterate from the end to the beginning of the list.
        c := get(list.tail);
        for i: 0..list.count-1 {
            `it_index := i;
            `it       := c.data;

            #insert body;
            c = get(c.previous);
        }
    }
    else {
        // Iterate from the start to the end of the list.
        c := get(list.head);
        for i: 0..list.count-1 {
            `it_index := i;
            `it        = c.data;

            #insert body;
            c = get(c.next);
        }
    }

}


main :: () {
    
    list: Relocatable_List(int);
    append(*list, 1);

    return;
}

#scope_file
#import "Basic";
#load "./relative-pointer.jai";

new_relocatable_node :: (data: $T) -> *Relocatable_List_Node(T) {

    size := size_of(Relocatable_List_Node(T));
    node := cast(*Relocatable_List_Node(T))alloc(size);
    
    node.data = data;
    set(*node.previous, null);
    set(*node.next,     null);
    
    return node;
}