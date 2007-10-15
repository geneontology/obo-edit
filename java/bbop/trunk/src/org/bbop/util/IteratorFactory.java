package org.bbop.util;

import java.util.Iterator;

public interface IteratorFactory<IN_TYPE, OUT_TYPE> {

	public Iterator<OUT_TYPE> getIterator(IN_TYPE object);
}
