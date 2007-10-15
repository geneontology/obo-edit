package org.bbop.util;

import java.util.Iterator;

public class IterableIteratorFactory implements IteratorFactory<Iterable, Object> {

	public Class<Iterable> getInType() {
		return Iterable.class;
	}

	public Iterator getIterator(Iterable object) {
		return object.iterator();
	}

	public Class getOutType() {
		return Object.class;
	}
}
