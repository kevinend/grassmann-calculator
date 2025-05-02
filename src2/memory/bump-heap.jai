#scope_export

DEFAULT_PAGE_SIZE :: 4096;

Bump_Heap :: struct {
    base_address:          *u8;
    current_position:      *u8;
    checkpoint_address:    *u8;
    next_uncommitted_page: *u8;
    max_address:           *u8;
}

init :: (heap: *Bump_Heap, size: int = DEFAULT_PAGE_SIZE) {
    
    Basic.assert(heap != null);
    reserve := align(size, DEFAULT_PAGE_SIZE);

    address := Windows.VirtualAlloc(null, cast(u64)reserve, Windows.MEM_RESERVE, Windows.PAGE_READWRITE);
    if !address {
        value, description := System.get_error_value_and_string();
        Basic.log_error("Virtual alloc in Bump_Heap failed to reserve memory with error %: %\n", value, description);
        Basic.exit;
    }

    heap.base_address          = address;
    heap.current_position      = address;
    heap.checkpoint_address    = address;
    heap.next_uncommitted_page = address;
    heap.max_address           = address + reserve;

    return;
}

init :: (heap: *Bump_Heap, start: *u8, end: *u8) {
    
    // Initialize with a *fixed* buffer managed by the allocator.
    Basic.assert(heap  != null);
    Basic.assert(start != null);
    Basic.assert(end   != null);

    heap.base_address          = start;
    heap.current_position      = start;
    heap.checkpoint_address    = start;
    heap.next_uncommitted_page = end;
    heap.max_address           = end;

    return;

}

allocate :: (heap: *Bump_Heap, size: int, alignment: int) -> *u8 {
    Basic.assert(heap != null);
    if heap.base_address == null {
        heap_init(heap, DEFAULT_PAGE_SIZE);
    }

    allocation_start := align(heap.current_position, alignment);
    allocation_end   := allocation_start + size;

    if allocation_end > heap.next_uncommitted_page {
        if allocation_end > heap.max_address {
            Basic.log_error("Bump heap is out of memory.\n");
            Basic.exit;
        }
        else {
            expand_committed_pages(heap, allocation_end);
        }
    }
    
    heap.current_position = allocation_end;
    return allocation_start;
}

set_checkpoint :: (heap: *Bump_Heap) {
    heap.checkpoint_address = heap.current_position;
}

reset_to_checkpoint :: (heap: *Bump_Heap) -> *u8 {
    heap.current_position = heap.checkpoint_address;
    return heap.current_position;
}

print :: (heap: Bump_Heap) {
    Basic.print("Base    == %\n",               heap.base_address);
    Basic.print("Current == %\n",               heap.current_position);
    Basic.print("Next uncommitted page == %\n", heap.next_uncommitted_page);
    Basic.print("Max address == %\n",           heap.max_address);
    Basic.print("Checkpoint address == %\n",    heap.checkpoint_address);

    Basic.print("Allocated == %\n",   heap.current_position - heap.base_address);
    Basic.print("Unallocated == %\n", heap.max_address      - heap.current_position);
    Basic.print("\n");
}

bump_heap_allocator_proc :: (mode: Allocator_Mode, requested_size: s64, old_size: s64, old_memory: *void, allocator_data: *void) -> *void {

    // Bump heap allocator does *not* support freeing or resizing at this time
    if #complete mode == {
        case .ALLOCATE; {
            heap := cast(*Bump_Heap)allocator_data;
            Basic.assert(heap != null);

            // old_memory size is used for the alignment of the request for allocations
            alignment := old_size;
            ptr := heap_allocate(heap, requested_size, alignment);
            return ptr;
        }
        case .FREE;   { return null; }
        case .RESIZE; { return null; }
        
        case .STARTUP;  { return null; }
        case .SHUTDOWN; { return null; }

        case .THREAD_START; #through;
        case .THREAD_STOP; {
            Basic.assert(false, "Multithreaded access is not supported by the bump heap.\n");
            return null;
        }
        
        case .CREATE_HEAP; #through;
        case .DESTROY_HEAP; {
            Basic.assert(false, "Create/Destroy heap are not supported by bump heap.\n");
            return null;
        }
       
        case .IS_THIS_YOURS; {
            return null;
        }
        
        case .CAPS; {
            return null;
        }
    }
}

#scope_file
Basic   :: #import "Basic";
System  :: #import "System";
Windows :: #import "Windows";

align :: inline (x: $T, alignment: int) -> T {
    Basic.assert(alignment == 1 || alignment % 2 == 0);
    return (x + alignment-1) & ~(alignment-1);
}

expand_committed_pages :: (heap: *Bump_Heap, address: *u8) {

    diff        := address - heap.next_uncommitted_page;
    commit_size := align(diff, DEFAULT_PAGE_SIZE);
    Windows.VirtualAlloc(heap.next_uncommitted_page, cast(u64) commit_size, Windows.MEM_COMMIT, Windows.PAGE_READWRITE);

    heap.next_uncommitted_page += commit_size;

    return;
}