package org.bbop.util;

import java.io.*;
import java.util.*;

import org.apache.log4j.*;

public class Subset<T> extends AbstractSet<T> implements Serializable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(Subset.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 8179875474046027783L;
	protected VectorFilter<T> filter;
	protected int size = -1;
	protected boolean containedMarked = false;
	protected Collection<T> set;
	protected boolean cacheSize;

	public Subset() {
		this(null, null, false);
	}

	public Subset(VectorFilter<T> filter, Collection<T> set) {
		this(filter, set, false);
	}

	public Subset(VectorFilter<T> filter, Collection<T> set, boolean cacheSize) {
		this.filter = filter;
		this.set = set;
		this.cacheSize = cacheSize;
		if (cacheSize)
			cacheSize();
	}

	protected class CachedSubsetIterator implements Iterator<T> {
		int index = 0;
		Iterator<T> internal = set.iterator();
		T lastObject;

		public boolean hasNext() {
			return index < size;
		}

		public T next() {
			do {
				lastObject = internal.next();
			} while (!filter.satisfies(lastObject));
			index++;
			return lastObject;
		}

		public void remove() {
			if (filter.satisfies(lastObject))
				internal.remove();
		}
	}

	public Iterator iterator() {
		if (cacheSize)
			return new CachedSubsetIterator();
		else {
			return new FilteredIterator(filter, set.iterator());
		}
	}

	public boolean remove(Object o) {
		if (filter.satisfies((T) o))
			return set.remove(o);
		else
			return false;
	}

	protected void markForRecategorize(Object o) {
		containedMarked = contains(o);
	}

	protected void recategorize(Object o) {
		if (contains(o)) {
			if (!containedMarked)
				size++;
		} else {
			if (containedMarked)
				size--;
		}
	}
	
	public boolean isEmpty() {
		Iterator<T> it = set.iterator();
		while(it.hasNext()) {
			if (filter.satisfies(it.next()))
				return false;
		}
		return true;
	}

	public void setFilter(VectorFilter<T> filter) {
		this.filter = filter;
	}

	public void setData(Collection<T> set) {
		this.set = set;
	}

	public boolean add(T o) {
		if (filter.satisfies(o))
			return set.add(o);
		else
			return false;
	}

	protected void updateRemove(T o) {
		if (filter.satisfies(o))
			size--;
	}

	protected void updateAdd(T o) {
		if (filter.satisfies(o))
			size++;
	}

	protected void cacheSize() {
		cacheSize(0);
		Iterator<T> it = set.iterator();
		while (it.hasNext()) {
			if (filter.satisfies(it.next()))
				size++;
		}
	}

	protected void cacheSize(int size) {
		this.size = size;
	}

	public int size() {
		if (!cacheSize)
			cacheSize();
		return size;
	}

	public boolean contains(Object o) {
		return filter.satisfies((T) o) && set.contains(o);
	}

	public boolean equals(Object o) {
		return o == Subset.this;
	}
}
