package org.bbop.util;

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.apache.log4j.*;

public class EmptyIterator<T> implements Iterator<T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EmptyIterator.class);
	
	public static EmptyIterator<?> EMPTY_ITERATOR = new EmptyIterator();
	
	private EmptyIterator() {}

    public boolean hasNext() {
        return false;
    }
    public T next() {
        throw new NoSuchElementException();
    }
    public void remove() {
        throw new UnsupportedOperationException();
    }
    
    public static <T> Iterator<T> emptyIterator() {
    	return (Iterator<T>) EMPTY_ITERATOR;
    }
}
