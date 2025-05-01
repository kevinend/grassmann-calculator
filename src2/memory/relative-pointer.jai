/*
    A relative pointer is a compact representation of a pointer instead of using the cpu's default pointer size.
    The idea is that the 'pointer' type points 'relative' to its own position.
    A 'u16' relative pointer can address 65535 bytes ahead of the location of the pointer itself.

    This representation has two primary benefits.
    1. Serialization. 
       A buffer whose elements use relative pointers can be memcmp'd around, written and read back from disk and all references are valid.
    2. Size. 
       On x64-based CPUs a pointer is 8 bytes and can address any valid virtual address. 
       Sometimes an application needs to point a small number of bytes away from its current location.
       Using a relative pointer a single byte could be used instead of 8 if the next element is within 256 bytes which cuts memory usage by 1/8th.

#scope_export

/* 
    Note:

    Relative pointers are typically used for data types that are used as an intrusive linked list.
    The data that points to the head of the 'list' should definitely store a count of the number of items in the list for iteration.
    Setting the relative pointer to null sets the offset to 0 and get(ptr) would return it's own address so you can't do the typical
    iteration loop like while current != null { do-stuff; }.
      
    You can obviously get around this and have a procedure that exits when offset is zero, while is_end(ptr) where 'is_end' checks for the offset
    zero but then your logic in the loop needs duplicated outside the loop to handle this extra case. You could also set a sentinel value to
    check against. Just keep track of the count and loop the intuitive way.

    List :: struct {
        head:  *Node;
        count: int; 
    }
    Node :: struct {
        value: int;
        next: Relative_Pointer(Node);
    }

    // Assume allocate_node returns *Node.
    node1 := allocate_node(value = 10);
    node2 := allocate_node(value = 20);
    node3 := allocate_node(value = 30);

    set(*node1.next, node2);
    set(*node2.next, node3);
    set(*node3.next, null);

    list: List;
    list.head = node1;
    list.count = 3;

    current := list.head;
    for i: 0..list.count-1 {
        print("Value == %\n", current.value);
        current = get(current.next);
    }
*/

Relative_Pointer :: struct (T: Type) {
    // reference can't exceed -32,768 to 32,767 bytes from the relative pointer's location
    offset: s16;
}

set :: inline (ptr: *Relative_Pointer($T), next: *T) {
    ptr.offset = 0;
    if next { ptr.offset = cast(s16)(cast(s64)next - cast(s64)ptr); }
}

get :: inline (ptr: Relative_Pointer($T)) -> *T {
    return cast(*T)(cast(*u8)*ptr.offset + ptr.offset);
}