package org.bbop.util;

import java.util.*;
import java.io.Serializable;

public class HashSubsetable extends LinkedHashSet implements Subsetable,
Serializable {

    /**
	 * 
	 */
	private static final long serialVersionUID = -999036291683126314L;
	protected HashSet subsets = new HashSet();

/*
    protected class Subset extends AbstractSet implements Serializable {
	VectorFilter filter;
	protected int size;
	protected boolean containedMarked = false;

	public Subset(VectorFilter filter) {
	    this.filter = filter;
	    cacheSize();
	}

	protected class SubsetIterator implements Iterator {
	    int index = 0;
	    Iterator internal = HashSubsetable.this.iterator();
	    Object lastObject;

	    public boolean hasNext() {
		return index < Subset.this.size;
	    }

	    public Object next() {
		do {
		    lastObject = internal.next();
		} while(!filter.satisfies(lastObject));
		index++;
		return lastObject;
	    }

	    public void remove() {
		if (filter.satisfies(lastObject))
		    internal.remove();
	    }
	}

	public Iterator iterator() {
	    return new SubsetIterator();
	}

	public boolean remove(Object o) {
	    if (filter.satisfies(o))
		return HashSubsetable.this.remove(o);
	    else
		return false;
	}

	protected void markForRecategorize(Object o) {
	    containedMarked = contains(o);
	}

	protected void recategorize(Object o) {
	    if (contains(o)) {
		if (!containedMarked)
		    Subset.this.size++;
	    } else {
		if (containedMarked)
		    Subset.this.size--;
	    }
	}

	public boolean add(Object o) {
	    if (filter.satisfies(o))
		return HashSubsetable.this.add(o);
	    else
		return false;
	}

	protected void updateRemove(Object o) {
	    if (filter.satisfies(o))
		Subset.this.size--;
	}

	protected void updateAdd(Object o) {
	    if (filter.satisfies(o))
		Subset.this.size++;
	}

	protected void cacheSize() {
	    cacheSize(0);
	    Iterator it = HashSubsetable.this.iterator();
	    while(it.hasNext()) {
		if (filter.satisfies(it.next()))
		    Subset.this.size++;
	    }
	}

	protected void cacheSize(int size) {
	    Subset.this.size = size;
	}

	public int size() {
	    return Subset.this.size;
	}

	public boolean contains(Object o) {
	    return filter.satisfies(o) &&
		HashSubsetable.this.contains(o);
	}

	public boolean equals(Object o) {
	    return o == Subset.this;
	}
    }
*/

    public Set getUncachedSubset(VectorFilter filter) {
	Subset out = new Subset(filter, this, false);
	subsets.add(out);
	return out;
    }

    public Set getSubset(VectorFilter filter) {
	Subset out = new Subset(filter, this, true);
	subsets.add(out);
	return out;
    }

    public boolean removeSubset(Set set) {
	return subsets.remove(set);
    }

    public void clear() {
	super.clear();
	Iterator it = subsets.iterator();
	while(it.hasNext()) {
	    Subset set = (Subset) it.next();
	    set.cacheSize(0);
	}
    }

    public boolean add(Object o) {
	boolean add = super.add(o);
	if (add) {
	    Iterator it = subsets.iterator();
	    while(it.hasNext()) {
		Subset set = (Subset) it.next();
		set.updateAdd(o);
	    }
	}
	return add;
    }

    public boolean remove(Object o) {
	boolean remove = super.remove(o);
	if (remove) {
	    Iterator it = subsets.iterator();
	    while(it.hasNext()) {
		Subset set = (Subset) it.next();
		set.updateRemove(o);
	    }
	}
	return remove;
    }

    public void markForRecategorize(Object o) {
	Iterator it = subsets.iterator();
	while(it.hasNext()) {
	    Subset set = (Subset) it.next();
	    set.markForRecategorize(o);
	}
    }

    public void recategorize(Object o) {
	Iterator it = subsets.iterator();
	while(it.hasNext()) {
	    Subset set = (Subset) it.next();
	    set.recategorize(o);
	}
    }

    public void recache(Set s) {
	if (subsets.contains(s)) {
	    ((Subset) s).cacheSize();
	}
    }

    public void recache() {
	Iterator it = subsets.iterator();
	while(it.hasNext()) {
	    Subset set = (Subset) it.next();
	    set.cacheSize();
	}	
    }
}
