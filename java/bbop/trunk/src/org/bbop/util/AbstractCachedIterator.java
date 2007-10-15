package org.bbop.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

public abstract class AbstractCachedIterator implements Iterator {
	
	protected Iterator iterator;
	protected Collection cache;
	
	public void setIterator(Iterator iterator) {
		this.iterator = iterator;
		cache = null;
	}
	
	protected void doCache() {
		cache = CollectionUtil.initialize(new LinkedList(), iterator);
		iterator = cache.iterator();
	}

	public boolean hasNext() {
		return iterator.hasNext();
	}

	public Object next() {
		return iterator.next();
	}

	public void remove() {
		throw new UnsupportedOperationException("CachedIterators are read-only");
	}

}
