/*
    Allocator manages a heap of memory with a simple pointer "bump".
    The memory allocated within the pointer-bump allocator should have similar lifetimes
    because freeing individual allocations is not supported. The entire allocator can be
    reset at any time. Following a reset new allocations will overwrite the memory of any
    previous allocations.
*/

#scope_export

DEFAULT_PAGE_SIZE :: 4096;

Bump_Allocator :: struct {

    current_position:      u64;
    base_address:          *u8;
    next_uncommitted_page: *u8;
    max_address:           *u8;
}

bump_allocator_init :: (bump: *Bump_Allocator, size: u64 = DEFAULT_PAGE_SIZE) {

    // Initialize the allocator with either a single page or the requested size as a multiple of the default page size.
    
    Basic.assert(bump != null);
    reserve := align(size, DEFAULT_PAGE_SIZE);

    address := Windows.VirtualAlloc(null, cast(u64)reserve, Windows.MEM_RESERVE, Windows.PAGE_READWRITE);
    if !address {
        value, description := System.get_error_value_and_string();
        Basic.log_error("Virtual alloc in poiner bump allocator failed to reserve memory with error %: %\n", value, description);
        Basic.exit;
    }

    bump.current_position      = 0;
    bump.base_address          = address;
    bump.next_uncommitted_page = address;
    bump.max_address           = address + reserve;

    return;
}

bump_allocator_init :: (bump: *Bump_Allocator, start: *u8, end: *u8) {
    
    // Initialize the allocator with preallocated memory.

    Basic.assert(bump  != null);
    Basic.assert(start != null);
    Basic.assert(end   != null);

    bump.current_position      = 0;
    bump.base_address          = start;
    bump.next_uncommitted_page = end;
    bump.max_address           = end;

    return;
}

bump_allocator_allocate :: (bump: *Bump_Allocator, size: u64, alignment: u64) -> *u8 {

    Basic.assert(bump != null);
    if bump.base_address == null {
        bump_allocator_init(bump, DEFAULT_PAGE_SIZE);
    }

    allocation_start := align(bump.current_position, alignment);
    allocation_end   := allocation_start + size;

    end := bump.base_address + allocation_end;
    if end > bump.next_uncommitted_page {
        if end > bump.max_address {
            Basic.log_error("Pointer bump allocator is out of memory.\n");
            Basic.exit;
        }
        else {
            expand_committed_pages(bump, end);
        }
    }

    user_ptr := bump.base_address + allocation_start;

    bump.current_position = allocation_end;
    return user_ptr;
}

bump_allocator_reset :: (bump: *Bump_Allocator) {

    bump.current_position = 0;
    return;
}

bump_allocator_print :: (bump: Bump_Allocator) {

    Basic.print("Reserved                   == %\n", cast(u64)bump.max_address - cast(u64)bump.base_address);
    Basic.print("Committed                  == %\n", cast(u64)bump.next_uncommitted_page - cast(u64)bump.base_address);
    Basic.print("Uncommitted                == %\n", cast(u64)bump.max_address           - cast(u64)bump.next_uncommitted_page);

    Basic.print("Allocated from committed   == %\n",  bump.current_position);
    Basic.print("Unallocated from committed == %\n", (cast(u64)bump.next_uncommitted_page - cast(u64)bump.base_address) - bump.current_position);

    Basic.print("Base address               == %\n", bump.base_address);
    Basic.print("Next uncommitted page      == %\n", bump.next_uncommitted_page);
    Basic.print("Max address                == %\n", bump.max_address);

    Basic.print("Current position           == %\n", bump.current_position);

    Basic.print("\n");
}

test :: () {

    bump: Bump_Allocator;
    xs := cast(*int)bump_allocator_allocate(*bump, size=size_of(int), alignment=size_of(int));
    ys := cast(*float)bump_allocator_allocate(*bump, size=5*size_of(float), alignment=size_of(float));

    bump_allocator_print(*bump);
    bump_allocator_reset(*bump);

    zs := cast(*u16)bump_allocator_allocate(*bump, size= 3*size_of(u16), alignment=size_of(u16));
    bump_allocator_print(*bump);

    bump2: Bump_Allocator;
    bump_allocator_init(*bump2, 4096*2);
    bump_allocator_print(*bump2);
}

#scope_file
Basic   :: #import "Basic";
System  :: #import "System";
Windows :: #import "Windows";

align :: inline (x: $T, alignment: u64) -> T {
    Basic.assert(alignment == 1 || alignment % 2 == 0);
    return (x + alignment-1) & ~(alignment-1);
}

expand_committed_pages :: (bump: *Bump_Allocator, address: *u8) {

    diff        := cast(u64)(address - bump.next_uncommitted_page);
    commit_size := align(diff, DEFAULT_PAGE_SIZE);
    Windows.VirtualAlloc(bump.next_uncommitted_page, cast(u64) commit_size, Windows.MEM_COMMIT, Windows.PAGE_READWRITE);

    bump.next_uncommitted_page += commit_size;

    return;
}