package org.bbop.util;

import java.util.Iterator;

import org.apache.log4j.*;

public class IterableIteratorFactory implements IteratorFactory<Iterable, Object> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IterableIteratorFactory.class);

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
