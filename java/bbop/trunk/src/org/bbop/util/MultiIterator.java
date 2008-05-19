package org.bbop.util;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.NoSuchElementException;

import org.apache.log4j.*;

public class MultiIterator implements Iterator {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiIterator.class);

	protected List iterators = new LinkedList();
	
	public void addIterator(Iterator it) {
		iterators.add(it);
	}
	
	public boolean hasNext() {
		if (iterators.size() == 0)
			return false;
		Iterator it = iterators.iterator();
		while(it.hasNext()) {
			Iterator current = (Iterator) it.next();
			if (current.hasNext())
				return true;
			else
				it.remove();			
		}
		return false;
	}

	public Object next() {
		if (iterators.size() == 0)
			throw new NoSuchElementException();
		Iterator it = iterators.iterator();
		while(it.hasNext()) {
			Iterator current = (Iterator) it.next();
			if (current.hasNext())
				return current.next();
			else
				it.remove();			
		}
		throw new NoSuchElementException();
	}

	public void remove() {
		throw new UnsupportedOperationException("Multi-iterators are read only");
	}
	
	public static void main(String [] args) {
		List a = new LinkedList();
		List b = new LinkedList();
		a.add("do");
		a.add("re");
		a.add("mi");
		a.add("fa");
		b.add("so");
		b.add("la");
		b.add("ti");
		b.add("do");
		MultiIterator it = new MultiIterator();
		it.addIterator(a.iterator());
		it.addIterator(b.iterator());
		while(it.hasNext()) {
			System.out.println(it.next());
		}
	}

}
