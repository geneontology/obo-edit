package org.bbop.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.apache.log4j.*;

public class FilteredIterator implements Iterator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FilteredIterator.class);

	protected boolean outOfGas = true;
	protected Object nextObject;
	protected VectorFilter filter;
	protected Iterator iterator;

	public boolean hasNext() {
		return !outOfGas;
	}

	public Object next() {
		Object out = nextObject;
		fetchNextObject();
		return out;
	}

	protected void fetchNextObject() {
		while(iterator.hasNext()) {
			Object o = iterator.next();
			if (filter.satisfies(o)) {
				outOfGas = false;
				nextObject = o;
				return;
			}
		}
		nextObject = null;
		outOfGas = true;
	}

	public void remove() {
		throw new UnsupportedOperationException(
				"Filtered iterators are read only");
	}

	public FilteredIterator() {

	}

	public FilteredIterator(VectorFilter filter, Iterator iterator) {
		setFilter(filter);
		setIterator(iterator);
	}

	public VectorFilter getFilter() {
		return filter;
	}

	public void setFilter(VectorFilter filter) {
		this.filter = filter;
		outOfGas = true;
		nextObject = null;
	}

	public Iterator getIterator() {
		return iterator;
	}

	public void setIterator(Iterator iterator) {
		this.iterator = iterator;
		fetchNextObject();
	}

	public static void main(String [] args) {
		VectorFilter filter = new VectorFilter() {
			public boolean satisfies(Object o) {
				return o.toString().startsWith("a");
			}
		};
		Collection list = new LinkedList();
		list.add("monkeylegs");
		list.add("armenia");
		list.add("aspergers");
		list.add("bolus");
		list.add("wurst");
		FilteredIterator it = new FilteredIterator(filter, list.iterator());
		while(it.hasNext()) {
			System.out.println(it.next());
		}
	}
}
