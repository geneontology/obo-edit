package org.bbop.util;

import java.util.Iterator;

public class TransformingIterator<IN, OUT> implements Iterator<OUT> {
	
	protected Iterator<IN> iterator;
	protected VectorTransformer<IN, OUT> transformer;
	
	public TransformingIterator(Iterator<IN> iterator, VectorTransformer<IN, OUT> transformer) {
		this.iterator = iterator;
		this.transformer = transformer;
	}

	public boolean hasNext() {
		return iterator.hasNext();
	}

	public OUT next() {
		IN obj = iterator.next();
		return transformer.transform(obj);
	}

	public void remove() {
		iterator.remove();
	}

}
