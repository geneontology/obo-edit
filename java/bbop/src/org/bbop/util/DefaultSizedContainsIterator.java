package org.bbop.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

public class DefaultSizedContainsIterator implements SizedContainsIterator {
	
	protected Iterator iterator;
	protected Collection cacheCollection;
	protected boolean doOwnCaching = false;
	
	public DefaultSizedContainsIterator(Iterator iterator, Collection cacheCollection) {
		this.iterator = iterator;
		this.doOwnCaching = cacheCollection == null;
		if (cacheCollection == null) {
			this.cacheCollection = new LinkedList();
		}
		this.cacheCollection = cacheCollection;
	}

	public int size() {
		// TODO Auto-generated method stub
		return 0;
	}

	public boolean hasNext() {
		// TODO Auto-generated method stub
		return false;
	}

	public Object next() {
		// TODO Auto-generated method stub
		return null;
	}

	public void remove() {
		// TODO Auto-generated method stub

	}

	public boolean contains(Object o) {
		// TODO Auto-generated method stub
		return false;
	}

}
