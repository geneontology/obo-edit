package org.bbop.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.apache.log4j.*;

public class SingletonIterator<T> implements Iterator<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SingletonIterator.class);
	
	protected boolean nexted = false;
	protected T object;
	
	public SingletonIterator(T object) {
		this.object = object;
	}

	public boolean hasNext() {
		return !nexted;
	}

	public T next() {
		if (nexted)
			throw new NoSuchElementException();
		nexted = true;
		return object;
	}

	public void remove() {
		throw new UnsupportedOperationException();
	}

}
