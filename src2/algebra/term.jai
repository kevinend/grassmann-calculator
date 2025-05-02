#scope_export

Term :: struct {
    value: float;
    #place value; multiples: float;

    // Factors include both numerators and denominators.
    // all 'count' fields *must* be signed to prevent underflow in for loops.
    factors: Relative_Pointer(Factor);
    factor_count: s16;                  

    previous: Relative_Pointer(Term);
    next:     Relative_Pointer(Term);
}

new_term :: (value: float, heap: *Bump_Heap) -> *Term {

    term := cast(*Term)heap_allocate(heap, size = size_of(Term), alignment = size_of(Term));
    assert(term != null);
    memset(term, 0, size_of(Term));

    term.value = value;
    return term;
}

set_next_term :: (term: *Term, next: *Term) {

    // adjust the term.next to point to the next term in the list.
    set(term.next, next);
    return;
}

set_previous_term :: (term: *Term, previous: *Term) {

    // adjust the term.previous to point to the previous term in the list.
    set(term.previous, previous);
    return;
}

get_next_term :: (term: *Term) -> *Term {
    return get(term.next);
}

get_previous_term :: (term: *Term) -> *Term {
    return get(term.previous);
}

get_term_size :: (term: *Term) -> int {
    start   := cast(*u8)term;
    factors := get(term.factors);
    end     := cast(*u8)(factors + term.factor_count);

    return end - start;
}

append_factor :: (factor: *Factor, term: *Term) {

    // TODO: 05.01.2025 -- How to handle Numerators and Denominators???
    assert(term   != null)
    assert(factor != null);
    if term.factor_count == 0 {
        set(*term.factors, factor);
    }
    
    term.factor_count += 1;
    return;
}

terms_are_equal :: (Lterm: *Term, Rterm: *Term) -> bool {
    
    // Terms are equal when the following conditions are true.
	//  1. Each term contains the same number of factors (even if the count is zero).
	//  2. Each term contains the same factors in the same order.
	//  3. Each factor in the term is raised to the same power.
	if Lterm.factor_count == Rterm.factor_count {
		count := Lterm.factor_count;
        Lfactors := cast(*u8)get(*Lterm.factors);
        Rfactors := cast(*u8)get(*Rterm.factors);

        comp  := memcmp(Lfactors, Rfactors, count * size_of(Factor)) == 0;
        return comp;
	}
	else {
		return false;
	}
}

#scope_file