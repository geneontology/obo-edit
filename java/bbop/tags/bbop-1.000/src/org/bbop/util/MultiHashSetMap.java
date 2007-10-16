package org.bbop.util;

import java.util.Collection;
import java.util.HashSet;

public class MultiHashSetMap<K,V> extends MultiHashMap<K,V> {

	protected Collection<V> createCollection() {
		return new HashSet<V>();
	}

}
