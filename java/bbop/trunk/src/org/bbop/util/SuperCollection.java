package org.bbop.util;

import java.util.AbstractCollection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.apache.log4j.*;

public class SuperCollection<IN, OUT> extends AbstractCollection<OUT> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SuperCollection.class);
	protected IN root;
	protected List<IteratorFactory> factories;
	
	public SuperCollection(IN root, IteratorFactory... factories) {
		this.root = root;
		this.factories = new LinkedList<IteratorFactory>();
		for(IteratorFactory factory : factories)
			this.factories.add(factory);
	}
	
	public SuperCollection(IN root, List<IteratorFactory> factories) {
		this.factories = factories;
		this.root = root;
	}

	@Override
	public Iterator<OUT> iterator() {
		return new SuperIterator<IN, OUT>(root, factories);
	}

	@Override
	public int size() {
		int i=0;
		Iterator<?> it = iterator();
		while(it.hasNext()) {
			i++;
			it.next();
		}
		return i;
	}

}
