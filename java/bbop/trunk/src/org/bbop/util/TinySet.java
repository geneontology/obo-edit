package org.bbop.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import org.apache.log4j.*;

public class TinySet<T> implements Set<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TinySet.class);

	protected Collection<T> list;

	protected void forceCollection() {
		if (list == null)
			list = new ArrayList<T>(1);
	}

	public boolean add(T o) {
		forceCollection();
		if (!list.contains(o)) {
			list.add(o);
			return true;
		} else
			return false;
	}

	public boolean addAll(Collection<? extends T> c) {
		forceCollection();
		boolean added = false;
		Iterator<? extends T> it = c.iterator();
		while (it.hasNext()) {
			T o = it.next();
			if (!list.contains(o)) {
				list.add(o);
				added = true;
			}
		}
		return added;
	}

	public void clear() {
		list = null;
	}

	public boolean contains(Object o) {
		if (list == null)
			return false;
		return list.contains(o);
	}

	public boolean containsAll(Collection<?> c) {
		if (list == null)
			return c.size() == 0;
		Iterator it = list.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (!list.contains(o))
				return false;
		}
		return true;
	}

	public boolean isEmpty() {
		return list == null;
	}

	protected static final Iterator emptyIterator = new Iterator() {
		public boolean hasNext() {
			return false;
		}

		public Object next() {
			throw new RuntimeException(
					"Cannot call next() on an empty iterator");
		}

		public void remove() {
			throw new RuntimeException(
					"Cannot call next() on an empty iterator");
		}

	};

	@SuppressWarnings("unchecked")
	public Iterator<T> iterator() {
		if (list == null)
			return emptyIterator;
		else
			return list.iterator();
	}

	public boolean remove(Object o) {
		if (list == null)
			return false;
		if (list.contains(o) && list.size() == 1) {
			list = null;
			return true;
		}
		return list.remove(o);
	}

	public boolean removeAll(Collection<?> c) {
		boolean removed = false;
		Iterator it = c.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (list.remove(o))
				removed = true;
			if (list == null)
				break;
		}
		return removed;
	}

	public boolean retainAll(Collection<?> c) {
		Iterator it = iterator();
		boolean changed = false;
		while (it.hasNext()) {
			Object o = it.next();
			if (!c.contains(o)) {
				it.remove();
				changed = true;
			}
		}
		if (list.size() == 0)
			list = null;
		return changed;
	}

	public int size() {
		if (list == null)
			return 0;
		else
			return list.size();
	}

	public Object[] toArray() {
		Object[] result = new Object[size()];
		Iterator<T> e = iterator();
		for (int i = 0; e.hasNext(); i++)
			result[i] = e.next();
		return result;
	}

	@SuppressWarnings("unchecked")
	public <E> E[] toArray(E[] a) {
		int size = size();
		if (a.length < size)
			a = (E[]) java.lang.reflect.Array.newInstance(a.getClass()
					.getComponentType(), size);

		Iterator<T> it = iterator();
		Object[] result = a;
		for (int i = 0; i < size; i++)
			result[i] = it.next();
		if (a.length > size)
			a[size] = null;
		return a;
	}

	public String toString() {
		if (list == null)
			return "[]";
		else
			return list.toString();
	}
}
