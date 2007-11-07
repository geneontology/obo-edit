package org.bbop.util;

import java.util.ArrayList;
import java.util.Collection;

public class MultiArrayListMap<K,V> extends MultiHashMap<K,V> {

	@Override
	protected Collection<V> createEmptyCollection() {
		return new ArrayList<V>();
	}
}
