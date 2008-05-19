package org.bbop.util;

import java.util.Collection;
import java.util.HashSet;

import org.apache.log4j.*;

public class MultiHashSetMap<K,V> extends MultiHashMap<K,V> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiHashSetMap.class);

	protected Collection<V> createCollection() {
		return new HashSet<V>();
	}

}
