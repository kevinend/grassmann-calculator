/*
    Allocator manages a heap of memory in a LIFO-like manner.
    Individual allocations can be reclaimed through a pop operation.

    There is a maximum number of allocations that can be handled by the allocator.
    This is an arbitrary upper-bound but chosen to be reasonable.
    
    The allocator uses a fixed-sized array to track metadata of the allocations instead of embedding header metadata
    prior to the allocation. Like any implementation this has benefits and tradeoffs.

    Benefits:
    1. Allocations can be tightly packed (provided there is no padding based on alignment requirements).
    2. Easier to track the rollback positions/locations for each allocation.
    3. Easier to store additional metadata about the allocations if necessary (size or a generation/version).
    
    Tradeoffs:
    1. Wasted memory to track the metadata of allocations.
    2. Fixed sized limit on the total number of allocations.
    Both of these could be fixed by making the stack polymorphic on the upper bound on the total number of allocations.

    Diatribe:
    Most implementations of stack based allocators that I've seen that embed metadata prior to the allocation seem "off".
    The implementation will align the user pointer but not the metadata header. On some systems that is a straight up crash when
    reading the unaligned metadata header. On others it will result in slower access times that the user might not be aware of.

    The rememdy to that is to align both the metadata *and* the user_pointer but padding might need introduced so now you need a
    'header' for your 'header' telling you how far away your metadata is from the aligned pointer you generated for the user.
    Then the question is how big should that meta-metadata be and does it need aligned...

    It just gets messy fast and I'd rather not be bothered. 
    Also, in the best case where you allocate near the set upper limit for max allocations you'd waste as much space tracking the same metadata information anyways.

    Unless there is a reason to change the current approach it will stay as is. 05.08.2025.
*/

#scope_export

DEFAULT_PAGE_SIZE :: 4096;
MAX_ALLOCATIONS   :: 128;

Checkpoint :: struct {

    position: u64;
    index:    u64;
}

Stack_Allocator :: struct {

    current_position:      u64;
    base_address:          *u8;
    next_uncommitted_page: *u8;
    max_address:           *u8;

    allocation_count: u64;
    rollback_checkpoints: [MAX_ALLOCATIONS] u64; // index 0 is unused!
}

stack_allocator_init :: (stack: *Stack_Allocator, size: u64 = DEFAULT_PAGE_SIZE) {

    // Initialize the stack with either a single page or the requested size as a multiple of the default page size.

    Basic.assert(stack != null);
    reserve := align(size, DEFAULT_PAGE_SIZE);

    address :=  Windows.VirtualAlloc(null, cast(u64)reserve, Windows.MEM_RESERVE, Windows.PAGE_READWRITE);
    if !address {
        value, description := System.get_error_value_and_string();
        Basic.log_error("Virtual alloc in Stack Allocator failed to reserve memory with error %: %\n", value, description);
        Basic.exit;
    }

    stack.current_position      = 0;
    stack.base_address          = address;
    stack.next_uncommitted_page = address;
    stack.max_address           = address + reserve;
    stack.allocation_count      = 0;

    return;
    
}

stack_allocator_init :: (stack: *Stack_Allocator, start: *u8, end: *u8) {

    // Initialize the stack with preallocated memory.

    Basic.assert(stack  != null);
    Basic.assert(start != null);
    Basic.assert(end   != null);

    stack.current_position      = 0;
    stack.base_address          = start;
    stack.next_uncommitted_page = end;
    stack.max_address           = end;
    stack.allocation_count      = 0;
}

stack_allocator_allocate :: (stack: *Stack_Allocator, size: u64, alignment: u64) -> *u8 {

    // 'Push' an allocation onto the end of the stack.

    Basic.assert(stack != null);
    Basic.assert(stack.allocation_count < MAX_ALLOCATIONS);

    if stack.base_address == null {
        stack_allocator_init(stack, DEFAULT_PAGE_SIZE);
    }

    stack.allocation_count += 1;
    stack.rollback_checkpoints[stack.allocation_count] = stack.current_position;

    allocation_start := align(stack.current_position, alignment);
    allocation_end   := allocation_start + size;

    end := stack.base_address + allocation_end;
    if end > stack.next_uncommitted_page {
        if end > stack.max_address {
            Basic.log_error("Stack allocator is out of memory.\n");
            Basic.exit;
        }
        else {
            expand_committed_pages(stack, end);
        }
    }

    stack.current_position = allocation_end;
    return stack.base_address + allocation_start;
}

stack_allocator_free :: (stack: *Stack_Allocator, ptr: *void) {

    // 'Pop' the last allocation from the head of the stack.

    if stack.allocation_count == 0 { return; }

    position := cast(u64)ptr - cast(u64)stack.base_address;
    previous := stack.rollback_checkpoints[stack.allocation_count];

    if position >= previous && position < stack.current_position {
        // Supplied pointer was the head of the stack.
        stack.current_position = stack.rollback_checkpoints[stack.allocation_count];
        stack.allocation_count -= 1;
    }
    else {
        Basic.log_error("Error, attempt to free/pop memory within the stack that is not the head allocation.\n");
        Basic.exit;
    }

    return;
}

get_checkpoint :: (stack: *Stack_Allocator) -> Checkpoint {
    checkpoint: Checkpoint;
    checkpoint.position = stack.current_position;
    checkpoint.index    = stack.allocation_count;

    return checkpoint;
}

reset_to_checkpoint :: (stack: *Stack_Allocator, checkpoint: Checkpoint) {
    stack.current_position = checkpoint.position;
    stack.allocation_count = checkpoint.index;
}

test :: () {

    stack: Stack_Allocator;
    stack_allocator_init(*stack);

    xs := cast(*int)stack_allocator_allocate(*stack, size_of(int), size_of(int));
    c  := get_checkpoint(*stack);
    ys := cast(*int)stack_allocator_allocate(*stack, 5 * size_of(int), size_of(int));
    zs := cast(*u16)stack_allocator_allocate(*stack, 5 * size_of(u16), size_of(u16));
    Basic.print("Stack allocator == %\n", stack);

    reset_to_checkpoint(*stack, c);

    Basic.print("Stack allocator == %\n", stack);
    stack_allocator_free(*stack, xs);
    Basic.print("Stack allocator == %\n", stack);
}

#scope_file
Basic   :: #import "Basic";
System  :: #import "System";
Windows :: #import "Windows";

align :: inline (x: $T, alignment: u64) -> T {
    Basic.assert(alignment == 1 || alignment % 2 == 0);
    return (x + alignment-1) & ~(alignment-1);
}

expand_committed_pages :: (stack: *Stack_Allocator, address: *u8) {

    diff        := cast(u64)(address - stack.next_uncommitted_page);
    commit_size := align(diff, DEFAULT_PAGE_SIZE);
    Windows.VirtualAlloc(stack.next_uncommitted_page, cast(u64) commit_size, Windows.MEM_COMMIT, Windows.PAGE_READWRITE);

    stack.next_uncommitted_page += commit_size;

    return;
}