package org.bbop.util;

import java.util.*;

public class FastSuperset<T> extends AbstractCollection<T> implements
		Superset<T> {

	protected List<Collection<? extends T>> subsets = new LinkedList<Collection<? extends T>>();

	protected Collection<T> modifyMe;
	
	protected class SupersetIterator implements Iterator<T> {

		Iterator<Collection<? extends T>> subsetIterator;

		Iterator<? extends T> currentIterator;

		Collection<? extends T> currentSet;
		
		//protected Throwable creationException;
		
		protected int index = 0;

		public SupersetIterator() {
			//creationException = new Exception();
			subsetIterator = subsets.iterator();
			if (subsetIterator.hasNext()) {
				currentSet = subsetIterator.next();
				currentIterator = currentSet.iterator();
			}
		}

		public boolean hasNext() {
			if (currentIterator == null)
				return false;
			if (currentIterator.hasNext())
				return true;

			while (subsetIterator.hasNext()) {
				currentSet = subsetIterator.next();
				index++;
				currentIterator = currentSet.iterator();
				if (currentIterator.hasNext())
					return true;
			}
			currentIterator = null;

			return false;
		}

		public T next() {
			if (currentIterator != null && currentIterator.hasNext()) {
				try {
					return currentIterator.next();
				} catch (ConcurrentModificationException ex) {
					ex.printStackTrace();
					throw ex;
				}
			} else
				throw new NoSuchElementException("The iterator it empty");
		}

		public void remove() {
			if (currentSet == modifyMe)
				currentIterator.remove();
			else
				throw new UnsupportedOperationException("Attempted to use "
						+ "iterator to modify " + "unmodifiable set");
		}
	}

	public Iterator<T> iterator() {
		return new SupersetIterator();
	}

	public boolean add(T o) {
		if (modifyMe == null)
			throw new UnsupportedOperationException(
					"This superset is immutable");
		else if (!contains(o)) {
			return modifyMe.add(o);
		} else
			return false;
	}

	public boolean remove(Object o) {
		if (modifyMe == null)
			throw new UnsupportedOperationException(
					"This superset is immutable");
		else
			return modifyMe.remove(o);
	}

	public boolean isEmpty() {
		Iterator<Collection<? extends T>> it = subsets.iterator();
		while (it.hasNext()) {
			Collection s = it.next();
			if (!s.isEmpty())
				return false;
		}
		return true;
	}

	public void clear() {
		if (modifyMe != null && subsets.size() == 1)
			modifyMe.clear();
		else
			throw new UnsupportedOperationException("This superset contains "
					+ "unmodifiable subsets, and cannot be cleared");
	}

	public boolean contains(Object o) {
		Iterator it = subsets.iterator();
		while (it.hasNext()) {
			Collection s = (Collection) it.next();
			if (s.contains(o))
				return true;
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	public void addSubset(Collection<? extends T> set, boolean modifiable) {
		if (modifiable)
			modifyMe = (Collection<T>) set;
		subsets.add(set);
	}

	public void addSubset(Collection<? extends T> set) {
		addSubset(set, false);
	}

	public boolean removeSubset(Collection<? extends T> set) {
		Iterator it = subsets.iterator();
		while (it.hasNext()) {
			Collection s = (Collection) it.next();
			if (s == set) {
				it.remove();
				if (modifyMe == set)
					modifyMe = null;
				return true;
			}
		}
		return false;
	}

	@SuppressWarnings("unchecked")
	public void setModifier(Collection<? extends T> set) {
		Iterator<Collection<? extends T>> it = subsets.iterator();
		while (it.hasNext()) {
			Collection<? extends T> s = it.next();
			if (s == set) {
				modifyMe = (Collection<T>) set;
				return;
			}
		}
		subsets.add(set);
		modifyMe = (Collection<T>) set;
	}

	public int size() {
		int size = 0;
		Iterator<Collection<? extends T>> it = subsets.iterator();
		while (it.hasNext()) {
			Collection s = it.next();
			size += s.size();
		}
		return size;
	}
}
